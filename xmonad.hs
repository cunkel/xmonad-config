{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

import System.IO
import System.Directory (getHomeDirectory)
import Control.Monad
import Data.Maybe (fromMaybe, isJust, maybe, listToMaybe)
import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Actions.OnScreen
import XMonad.Actions.RotSlaves

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize

import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane

import XMonad.Util.Paste
import XMonad.Util.Run (spawnPipe)


-- Debugging

ldebug :: String -> IO ()
ldebug s = do
  home <- getHomeDirectory
  withFile (home ++ "/.xmonad/debug.log") AppendMode $ (flip hPutStrLn) s

xdebug :: String -> X()
xdebug = io . ldebug


-- Helpers for binding new keys

type KeyBindings = XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
type ModmBindingList = KeyMask -> [((KeyMask, KeySym), X ())]

bindKeys :: KeyBindings -> XConfig l -> KeyBindings
bindKeys ks source = newkeys
  where
    newkeys conf = ks conf `M.union` keys source conf

-- Accepts new key bindings mapped from base modifier to list, factoring out
-- M.fromList and XMonad.modMask from specfiying bindings.
bindModmList :: ModmBindingList -> XConfig l -> KeyBindings
bindModmList ks source = bindKeys newkeys source
  where
    newkeys conf = M.fromList $ ks (XMonad.modMask conf)

addKeys :: KeyBindings -> XConfig l -> XConfig l
addKeys ks source = source { keys = bindKeys ks source }

addModmList :: ModmBindingList -> XConfig l -> XConfig l
addModmList ks source = addKeys newkeys source
  where
    newkeys conf = M.fromList $ ks $ XMonad.modMask conf


-- Easily start commands in a series of windows in quick succession

enterNext :: X ()
enterNext = do
    sendKey noModMask xK_Return
    windows W.focusDown

upEnterNext :: X ()
upEnterNext = do
    sendKey noModMask xK_Up
    enterNext

enterNextKeys :: ModmBindingList
enterNextKeys modm = [((modm .|. controlMask, xK_backslash), upEnterNext),
                      ((modm,  xK_backslash), enterNext)]

useEnterNext :: XConfig l -> XConfig l
useEnterNext = addModmList enterNextKeys


-- Struts (docks)
--
-- NB: The layout must still be decorated with avoidStruts, as (at least
-- without FlexibleContexts) useStruts can't modify the layout hook of the
-- config.

strutsKeys :: ModmBindingList
strutsKeys modm = [((modm .|. shiftMask, xK_t), sendMessage ToggleStruts)]

useStruts :: XConfig l -> XConfig l
useStruts = docks . addModmList strutsKeys


-- Bind to Expand to S-; in VirtualBox in windows because S-l is the Windows
-- hotkey for lock screen.  Bind S-l to no action as we also get the event and
-- we don't want both Windows and our clients to receive it.

alternateExpandKeys :: ModmBindingList
alternateExpandKeys modm = [
  ((mod4Mask, xK_l), return ()),
  ((modm, xK_semicolon), sendMessage Expand)
  ]

avoidWindowsHotKeys :: XConfig l -> XConfig l
avoidWindowsHotKeys = addModmList alternateExpandKeys


-- Add extra workspaces (e.g. "0" / xK_0)

extraWorkspaces :: [(String, KeySym)] -> XConfig l -> XConfig l
extraWorkspaces ws source =
  source { keys = bindKeys newkeys source,
           workspaces = workspaces source ++ names }
  where
    names = map fst ws
    extraWsBinding m f = [((m, k), windows $ f w) | (w, k) <- ws]
    newkeys conf = M.fromList $
                   ((extraWsBinding (modMask conf) W.greedyView)
                     ++ (extraWsBinding ((modMask conf) .|. shiftMask) W.shift))


-- Add more per-workspace bindings

perWsBinding :: KeyMask -> (WorkspaceId -> WindowSet -> WindowSet) -> [(WorkspaceId, KeySym)] -> M.Map (KeyMask, KeySym) (X ())
perWsBinding m f ws = M.fromList [((m, k), windows $ f w) | (w, k) <- ws]

standardWorkspaceKeys :: [KeySym]
standardWorkspaceKeys = [xK_1 .. xK_9]

perWorkspaceKey :: KeyMask -> [(WorkspaceId, KeySym)] -> (WorkspaceId -> WindowSet -> WindowSet) -> XConfig l -> XConfig l
perWorkspaceKey modm extras f source = source { keys = newkeys }
    where
        ws conf = zip (workspaces conf) standardWorkspaceKeys ++ extras
        bindings conf = perWsBinding (modMask conf .|. modm) f (ws conf)
        newkeys = bindKeys bindings source


-- Key bindings for ResizeableTall

resizeableTallKeys :: ModmBindingList
resizeableTallKeys modm = [((modm, xK_z), sendMessage MirrorShrink),
                           ((modm, xK_a), sendMessage MirrorExpand)]

useResizeableTall :: XConfig l -> XConfig l
useResizeableTall = addModmList resizeableTallKeys


-- Key bindings for toggles

reflectKeys :: ModmBindingList
reflectKeys modm = [((modm .|. controlMask,  xK_x), sendMessage $ Toggle REFLECTX),
                    ((modm .|. controlMask,  xK_y), sendMessage $ Toggle REFLECTY)]

reflectToggles :: XConfig l -> XConfig l
reflectToggles = addModmList reflectKeys

spaceToggleKeys :: ModmBindingList
spaceToggleKeys modm = [
  ((modm, xK_space), sendMessage $ Toggle NBFULL),
  ((modm .|. controlMask, xK_space), sendMessage $ Toggle MIRROR)
  ]

spaceToggles :: XConfig l -> XConfig l
spaceToggles = addModmList spaceToggleKeys


-- xscreensaver lock via XF86 Calculator
--
-- XF86LogOff on my MS Wireless Natural Multimedia Keyboard has a keycode > 255
-- so doesn't get through, so I use XF86 Calculator instead.

calculatorLockKeys :: ModmBindingList
calculatorLockKeys modm = [((0, stringToKeysym "XF86Calculator"),
                           spawn "xscreensaver-command -lock")]

calculatorKeyLocks :: XConfig l -> XConfig l
calculatorKeyLocks = addModmList calculatorLockKeys


-- Change mod-Tab, mod-shift-Tab to rotate slaves instead of changing focus
-- (I use mod-j/mod-k to move focus)

tabRotatesKeys :: ModmBindingList
tabRotatesKeys modm = [
  ((modm, xK_Tab), rotSlavesUp),
  ((modm .|. shiftMask, xK_Tab), rotSlavesDown)
  ]

tabRotatesSlaves :: XConfig l -> XConfig l
tabRotatesSlaves = addModmList tabRotatesKeys


-- Screen splitting (and rescreening to undo)

screenSplittingKeys :: X () -> ModmBindingList
screenSplittingKeys doSplit modm = [
  ((modm .|. controlMask, xK_grave), rescreen),
  ((modm .|. controlMask .|. shiftMask,  xK_grave), doSplit)
  ]

splitScreens doSplit = addModmList (screenSplittingKeys doSplit)


-- Screen swapping

-- previous screen, except on screen 0, for which next
nearbyTarget :: ScreenId -> ScreenId
nearbyTarget x = if x <= (fromIntegral 0) then (fromIntegral 1) else x - (fromIntegral 1)

swapNearbyScreen :: X()
swapNearbyScreen = do
   ws <- gets windowset
   let now = W.screen (W.current ws)
       oneScreen = null $ tail $ W.screens ws
       s = if oneScreen then now else nearbyTarget now
   mws <- screenWorkspace s

   case mws of
       Nothing -> return ()
       Just tws -> windows (W.greedyView tws)

swapScreensKeys modm = [((modm, xK_grave), swapNearbyScreen)]
swapScreens = addModmList swapScreensKeys

-- Minimization (and unminimization)
--
-- It turns out I hate this and don't use it.

minimizeKeys modm = [
    ((modm, xK_m), withFocused minimizeWindow),
    ((modm .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
    ]

minimize :: XConfig l -> XConfig l
minimize conf = conf {
        keys = bindModmList minimizeKeys conf,
        handleEventHook = minimizeEventHook <+> handleEventHook conf
    }


-- Historical focus feature


{- HFState - State for historical focus feature.

history maps workspace tags to windows, sorted by how recently they have
been the master on that workspace.  The current master is the head of the list,
the second element the previous master, etc.

highlighting is the window whose border we have currently color to indicate
that it is the current swap target.

targetBorder is the border color we use to indicate the current swap target.
If it is Nothing it is not initialized.  If it is Just Nothing then initColor
with the specified Color failed.

-}

data HFState = HFState
  { history :: M.Map WorkspaceId [Window],
    highlighting :: Maybe Window,
    targetBorder :: Maybe (Maybe Pixel)
  } deriving (Typeable,Read,Show)

instance ExtensionClass HFState where
  initialValue = HFState M.empty Nothing Nothing
  extensionType = PersistentExtension

prioritize :: Eq a => [a] -> [a] -> [a]
prioritize xs ys = xs ++ filter (`notElem` xs) ys

-- Update the history based on the currently focused workspace:  put the master
-- window (if any) at the front of its mapping.
updateHistory  ::
  (Ord k, Ord a) =>
     M.Map k [a] -> W.StackSet k l a sid sd -> M.Map k [a]
updateHistory history ss =
  M.insertWith prioritize tag master history
  where
    tag = W.tag $ W.workspace $ W.current ss
    floating w = isJust $ M.lookup w (W.floating ss)
    master = take 1 $ filter (not . floating) (W.index ss)

data ToSwap a
    = NoSwap          -- nothing to do
    | SwapWithMaster  -- focus/position swap with master
    | SwapWith a      -- focus/position swap with specific window
    deriving (Show)

-- What to swap (focus or position) with.  If a non-master window is focused,
-- then swap with master (as swapMaster or focusMaster would).  If not, swap
-- with the window on the workspace that was most recently master (on this
-- workspace.)  If no such window, do nothing.  Floats do not participate.
swapTarget ::
  (Eq a, Ord a, Ord i) =>
    M.Map i [a] -> W.StackSet i l a sid sd -> ToSwap a
swapTarget hist ss
  | floating' focused = NoSwap
  | unfocusedMaster   = SwapWithMaster
  | null candidates   = NoSwap
  | otherwise         = SwapWith (head candidates)
  where ws = W.workspace $ W.current ss
        focused = W.peek ss
        floating' mw = isJust $ mw >>= (`M.lookup` W.floating ss)
        floating w = floating' $ Just w
        unfocusedMaster = isJust master && (master /= focused)
        onWorkspace = filter (not . floating) (W.index ss)
        master = listToMaybe onWorkspace
        tag = W.tag ws
        isCandidate a = (Just a /= master) && (elem a onWorkspace)
        candidates = filter isCandidate (M.findWithDefault [] tag hist)

setBorder :: Window -> Pixel -> X ()
setBorder win pixel = withDisplay $ \dpy -> io (setWindowBorder dpy win pixel)

maybeSetBorder :: Maybe Window -> Maybe Pixel -> X ()
maybeSetBorder win pixel = fromMaybe (return ()) $ (liftM2 setBorder) win pixel

setNormalBorder :: Window -> X ()
setNormalBorder win = asks normalBorder >>= setBorder win

setFocusedBorder :: Window -> X ()
setFocusedBorder win = asks focusedBorder >>= setBorder win

getTargetBorder :: String -> HFState -> X (Maybe Pixel)
getTargetBorder color state = do
  case targetBorder state of
    Just x  -> return x
    Nothing -> withDisplay $ \dpy -> io (initColor dpy color)

debugCurrentStack ss = do
  let floating w = isJust $ M.lookup w (W.floating ss)
  let floattag w = if floating w then " (floating)" else ""
  let focused w = W.peek ss == Just w
  let focusedtag w = if focused w then " (focused)" else ""

  let showOne w = xdebug ("  " ++ show w ++ floattag w ++ focusedtag w)

  xdebug "windows:"
  sequence $ map showOne $ W.index ss

dont x = return ()

-- The log hook does two things: it updates the history, and it updates border
-- colors so that the swap target is bordered by the specified color.
hfLogHook :: String -> X ()
hfLogHook color = do
  w <- gets windowset
  state <- XS.get
  targetBorderPixel <- getTargetBorder color state

  -- let newHistory = foldl updateHistory (history state) (W.workspaces w)
  let newHistory = updateHistory (history state) w

  let oldHighlight = highlighting state
      newHighlight = swapHighlight (swapTarget newHistory w) w
      isFocused = fromMaybe False ((liftM2 (==)) oldHighlight (W.peek w))
      restoreBorder = if isFocused then setFocusedBorder else setNormalBorder

  dont $ do
    xdebug ""
    debugCurrentStack w
    xdebug $ show (M.findWithDefault [] (W.tag $ W.workspace $ W.current w) newHistory)
    xdebug $ show (swapTarget newHistory w)
    xdebug $ "highlight: " ++ (show oldHighlight) ++ " -> " ++ (show newHighlight)

  when (oldHighlight /= newHighlight) $ maybe (return ()) restoreBorder oldHighlight

  maybeSetBorder newHighlight targetBorderPixel

  XS.put $ HFState newHistory newHighlight (Just targetBorderPixel)

currentMaster ss =
  listToMaybe $ filter (not . floating) (W.index ss)
  where
    floating w = isJust $ M.lookup w (W.floating ss)

-- window to highlight for ToSwap
swapHighlight :: (Eq a, Ord a) => ToSwap a -> W.StackSet i l a s sd -> Maybe a
swapHighlight (NoSwap) ss         = Nothing
swapHighlight (SwapWithMaster) ss = currentMaster ss
swapHighlight (SwapWith a) ss     = Just a

-- X action to swap position for ToSwap
swapAction :: ToSwap Window -> WindowSet -> WindowSet
swapAction (NoSwap)         = id
swapAction (SwapWithMaster) = W.swapMaster
swapAction (SwapWith a)     = W.swapMaster . W.focusWindow a

-- X action for new focus for ToSwap
focusAction :: ToSwap Window -> WindowSet -> WindowSet
focusAction (NoSwap)         = id
focusAction (SwapWithMaster) = W.focusMaster
focusAction (SwapWith a)     = W.focusWindow a

doSwapAction :: (ToSwap Window -> WindowSet -> WindowSet) -> X()
doSwapAction f = do
  state <- XS.get
  windows $ \ss -> f (swapTarget (history state) ss) ss

historicalSwapMaster :: X ()
historicalSwapMaster = doSwapAction swapAction

historicalFocusMaster :: X ()
historicalFocusMaster = doSwapAction focusAction

historicalFocusKeys :: ModmBindingList
historicalFocusKeys modm = [
  ((modm, xK_Return), historicalSwapMaster)
  , ((modm, xK_m ), historicalFocusMaster)

  -- Control added to default bindings
  , ((modm .|. controlMask,  xK_Return), windows W.swapMaster)
  , ((modm .|. controlMask,  xK_m ), windows W.focusMaster)
  ]

focusHistory :: String -> XConfig l -> XConfig l
focusHistory color conf = conf {
  keys = bindModmList historicalFocusKeys conf,
  logHook = hfLogHook color >> logHook conf
  }


-- Layout and associated features

myLayout = avoidStruts
  $ mkToggle(single REFLECTY)
  $ mkToggle(single REFLECTX)
  $ mkToggle(single NBFULL)
  $ mkToggle(single MIRROR)
  resizeable
  where
    resizeable = ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio = 1/2
    delta = 3/100

-- I'm trying using MIRROR and NBFULL toggles, with mod-Space toggling NBFULL
-- and mod-control-space toggling MIRROR, instead of cycling through these
-- three layouts.  I don't use Mirror resizeable a lot.  Verdict not yet in.
--
--    $ (resizable ||| Mirror resizable ||| noBorders Full)

layoutFeatures :: [XConfig l -> XConfig l]
layoutFeatures = [useStruts, reflectToggles, spaceToggles, useResizeableTall]


-- Other features

myExtraWorkspaces :: [(String, KeySym)]
myExtraWorkspaces = [("0", xK_0)]

-- This is how I split a 30 inch 2560 x 1600 display
split2560 = layoutSplitScreen 2 (TwoPane (3/100) (1/3))

-- This is how I split a 40 inch 3840 x 2160 display
split4k = layoutSplitScreen 2 (TwoPane (3/100) (1/2))

features :: [XConfig l -> XConfig l]
features = [
  focusHistory "darkred"
  , useEnterNext
  , extraWorkspaces myExtraWorkspaces
  , perWorkspaceKey controlMask myExtraWorkspaces (viewOnScreen 0)
  , tabRotatesSlaves
  , splitScreens split2560
  , swapScreens

--  , calculatorKeyLocks
  , avoidWindowsHotKeys
  ]


main = do
  xmobarProc <- spawnPipe "/usr/bin/xmobar"

  let xmobarLogHook = dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn xmobarProc,
        ppTitle = xmobarColor "green" "" . shorten 100
        }

  let base = def { modMask = mod4Mask
                 , terminal = "urxvt"
                 , borderWidth = 3
                 , layoutHook = myLayout
                 , logHook = xmobarLogHook
                 , focusFollowsMouse = True
                 }

  -- Features are applied left to right.
  let withFeatures = foldl (flip id) base (layoutFeatures ++ features)

  xmonad withFeatures
