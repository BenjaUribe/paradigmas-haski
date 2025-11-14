module Main where

import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.State
import qualified Data.Set as Set
import System.Exit (exitSuccess)

-- =============================================================================
-- CONFIGURACIÓN DE LA VENTANA
-- =============================================================================

-- Dimensiones de la ventana
windowWidth, windowHeight :: Float
windowWidth = 1280
windowHeight = 720

-- ventana del juego con dimensiones 1280x720
window :: Display
window = InWindow "Las flipantes aventuras del Haski-Mundo" (round windowWidth, round windowHeight) (100, 100)

backgroundColor :: Color
backgroundColor = makeColorI 20 20 40 255  -- Azul oscuro

fps :: Int
fps = 60

-- =============================================================================
-- ESTADO DEL JUEGO CON GRÁFICOS
-- =============================================================================

-- Estados/Escenas del juego
data GameScene = MainMenu | InGame | CreditsMenu deriving (Show, Eq)

-- Estado completo del juego
data GameWorld = GameWorld
    { currentScene :: GameScene           -- Escena actual
    , worldPlayer :: Player              -- Jugador
    , selectedMenuOption :: Int          -- Opción seleccionada en menús
    , selectedAction :: Int              -- Acción seleccionada en combate (0=Atacar, 1=Bloquear, 2=Escapar)
    , shouldExit :: Bool                 -- Indica si se debe salir del juego
    } deriving (Show)

-- Estado inicial del mundo
initialWorld :: CharacterClass -> GameWorld
initialWorld chosenClass = GameWorld
    { currentScene = MainMenu
    , selectedMenuOption = 0
    , worldPlayer = createPlayer chosenClass
    , selectedAction = 0
    , shouldExit = False
    }

-- =============================================================================
-- RENDERIZADO
-- =============================================================================

-- Renderizar el mundo completo según la escena actual
render :: GameWorld -> Picture
render world = case currentScene world of
    MainMenu -> renderMainMenu world
    InGame -> renderGame world
    CreditsMenu -> renderCreditsMenu world

-- Renderizar menú principal
renderMainMenu :: GameWorld -> Picture
renderMainMenu world = pictures
    [ translate 0 100 $ scale 0.5 0.5 $ color white $ text "HASKI-MUNDO"
    , renderMenuOptions ["Jugar", "Creditos", "Salir"] (selectedMenuOption world)
    , translate 0 (-200) $ scale 0.2 0.2 $ color (greyN 0.7) $ text "Usa flechas y Enter"
    ]

-- Renderizar el juego (solo fondo y HUD)
renderGame :: GameWorld -> Picture
renderGame world = pictures
    [ drawGameHUD world
    ]

-- Renderizar menú de créditos
renderCreditsMenu :: GameWorld -> Picture
renderCreditsMenu world = pictures
    [ -- Fondo sólido oscuro
      color (makeColorI 40 40 80 255) $ rectangleSolid windowWidth windowHeight
    , -- Título
      translate (-600) 250 $ scale 0.5 0.5 $ color white $ text "Creditos"
    , -- Créditos/nombres (movidos a la izquierda)
      translate (-450) 100 $ scale 0.3 0.3 $ color yellow $ text "Desarrollado por:"
    , translate (-450) 50 $ scale 0.25 0.25 $ color white $ text "Benjamin Uribe"
    , translate (-450) 20 $ scale 0.25 0.25 $ color white $ text "Marcelo Rojas"
    , translate (-450) (-10) $ scale 0.25 0.25 $ color white $ text "Elias Ojeda"
    , translate (-450) (-40) $ scale 0.25 0.25 $ color white $ text "Leonardo Moreno"
    , -- Botón volver
      translate 0 (-220) $ scale 0.3 0.3 $ color green $ text "Presiona ESC para volver"
    ]

-- Renderizar opciones de menú con selección
renderMenuOptions :: [String] -> Int -> Picture
renderMenuOptions options selected = pictures $
    zipWith (\i option -> 
        let yPos = 50 - fromIntegral i * 50
            textColor = if i == selected then yellow else white
            prefix = if i == selected then "> " else "  "
        in translate 0 yPos $ scale 0.3 0.3 $ color textColor $ text (prefix ++ option)
    ) [0..] options

-- HUD del juego con franja inferior y paneles
drawGameHUD :: GameWorld -> Picture
drawGameHUD world = pictures
    [ drawBottomBar
    , drawStatsPanel world
    , drawDicePanel world
    , drawActionsPanel world
    ]

-- Franja oscura en la parte inferior de 240px de altura
drawBottomBar :: Picture
drawBottomBar = 
    let barY = -240  -- Posición Y para que esté en la parte inferior
        barHeight = 240
    in pictures
        [ -- Fondo de la franja
          translate 0 barY $ color (makeColorI 15 15 15 255) $ 
          rectangleSolid windowWidth barHeight
        , -- Borde superior de la franja
          translate 0 (-120) $ color (makeColorI 60 60 60 255) $ 
          rectangleSolid windowWidth 4
        ]

-- Panel de estadísticas del jugador (izquierda, dentro de la franja)
drawStatsPanel :: GameWorld -> Picture
drawStatsPanel world = 
    let player = worldPlayer world
        panelX = -500  -- Más cerca del border
        panelY = -240  -- Centrado en la franja inferior
        panelWidth = 240  -- Más pequeño para estética
        panelHeight = 160  -- Más pequeño, cabe en la franja
    in pictures
        [ -- Fondo del panel
          translate panelX panelY $ color (makeColorI 40 40 40 220) $ 
          rectangleSolid panelWidth panelHeight
        , -- Borde del panel
          translate panelX panelY $ color white $ 
          rectangleWire panelWidth panelHeight
        , -- Título del panel
          translate (panelX - 100) (panelY + 45) $ scale 0.18 0.18 $ color white $ 
          text "ESTADISTICAS"
        , -- Línea separadora
          translate panelX (panelY + 35) $ color (greyN 0.6) $ 
          rectangleSolid (panelWidth - 20) 1
        , -- Estadísticas
          translate (panelX - 100) (panelY + 15) $ scale 0.14 0.14 $ color yellow $ 
          text ("Clase: " ++ show (playerClass player))
        , translate (panelX - 100) (panelY - 5) $ scale 0.14 0.14 $ color white $ 
          text ("Vida: " ++ show (playerHealth player))
        , translate (panelX - 100) (panelY - 25) $ scale 0.14 0.14 $ color white $ 
          text ("Danio: " ++ show (playerDamage player))
        , translate (panelX - 100) (panelY - 45) $ scale 0.14 0.14 $ color white $ 
          text ("Velocidad: " ++ show (playerSpeed player))
        ]

-- Panel de los dados
drawDicePanel :: GameWorld -> Picture
drawDicePanel world =
    let panelX = 0      -- Centrado
        panelY = -240   -- Centrado en la franja inferior
        panelWidth = 300
        panelHeight = 160
    in pictures
        [ -- Fondo del panel
          translate panelX panelY $ color (makeColorI 40 40 40 220) $ 
          rectangleSolid panelWidth panelHeight
        , -- Borde del panel
          translate panelX panelY $ color white $ 
          rectangleWire panelWidth panelHeight
        , -- Título del panel
          translate (panelX - 120) (panelY + 45) $ scale 0.18 0.18 $ color white $ 
          text "DADOS"
        , -- Línea separadora
          translate panelX (panelY + 35) $ color (greyN 0.6) $ 
          rectangleSolid (panelWidth - 20) 1
        , -- Información de los dados (placeholder)
          translate (panelX - 120) (panelY + 15) $ scale 0.14 0.14 $ color white $ 
          text "Dado de ataque: 1d20"
        , translate (panelX - 120) (panelY - 5) $ scale 0.14 0.14 $ color white $ 
          text "Dado de daño: 1d8"
        ]

-- Panel de acciones del jugador con botones (derecha, dentro de la franja)
drawActionsPanel :: GameWorld -> Picture
drawActionsPanel world = 
    let panelX = 500   -- Más cerca del borde
        panelY = -240  -- Centrado en la franja inferior
        panelWidth = 240   -- Más pequeño para estética
        panelHeight = 160  -- Más pequeño, cabe en la franja
    in pictures
        [ -- Fondo del panel
          translate panelX panelY $ color (makeColorI 40 40 40 220) $ 
          rectangleSolid panelWidth panelHeight
        , -- Borde del panel
          translate panelX panelY $ color white $ 
          rectangleWire panelWidth panelHeight
        , -- Título del panel
          translate (panelX - 100) (panelY + 55) $ scale 0.18 0.18 $ color white $ 
          text "ACCIONES PLAYER"
        , -- Línea separadora
          translate panelX (panelY + 35) $ color (greyN 0.6) $ 
          rectangleSolid (panelWidth - 20) 1
        , -- Botones de acción
          drawActionButtons panelX panelY (selectedAction world)
        ]

-- Renderizar botones de acción con selección (centrados y más grandes)
drawActionButtons :: Float -> Float -> Int -> Picture
drawActionButtons panelX panelY selected = pictures $
    zipWith (\i action -> 
        let yPos = panelY + 15 - fromIntegral i * 35  -- Más espacio entre botones
            isSelected = i == selected
            buttonColor = if isSelected then makeColorI 60 60 100 255 else makeColorI 30 30 30 255
            textColor = if isSelected then yellow else white
            prefix = if isSelected then "> " else "  "
            -- Botones más grandes y centrados
            buttonWidth = 200
            buttonHeight = 28
        in pictures
            [ -- Fondo del botón (centrado en el panel)
              translate panelX yPos $ color buttonColor $ 
              rectangleSolid buttonWidth buttonHeight
            , -- Borde del botón
              translate panelX yPos $ color (if isSelected then white else greyN 0.5) $ 
              rectangleWire buttonWidth buttonHeight
            , -- Texto del botón (centrado y un poco más abajo)
              translate (panelX - 80) (yPos - 7) $ scale 0.18 0.18 $ color textColor $ 
              text (prefix ++ action)
            ]
    ) [0..] ["Atacar", "Bloquear", "Escapar"]

-- =============================================================================
-- MANEJO DE EVENTOS
-- =============================================================================

-- Manejar entrada del teclado según la escena
handleInput :: Event -> GameWorld -> GameWorld
handleInput event world = case currentScene world of
    MainMenu -> handleMenuInput event world
    InGame -> handleGameInput event world
    CreditsMenu -> handleCreditsInput event world

-- Manejo de eventos en el menú principal
handleMenuInput :: Event -> GameWorld -> GameWorld
handleMenuInput (EventKey (SpecialKey KeyUp) Down _ _) world =
    world { selectedMenuOption = max 0 (selectedMenuOption world - 1) }
handleMenuInput (EventKey (SpecialKey KeyDown) Down _ _) world =
    world { selectedMenuOption = min 2 (selectedMenuOption world + 1) }
handleMenuInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
    case selectedMenuOption world of
        0 -> world { currentScene = InGame }  -- Jugar
        1 -> world { currentScene = CreditsMenu, selectedMenuOption = 0 }  -- creditos
        2 -> world { shouldExit = True }  -- Marcar para salir
        _ -> world
handleMenuInput _ world = world

-- Manejo de eventos en el juego (navegación de botones de acción)
handleGameInput :: Event -> GameWorld -> GameWorld
handleGameInput (EventKey (SpecialKey KeyEsc) Down _ _) world =
    world { currentScene = MainMenu, selectedMenuOption = 0 }
handleGameInput (EventKey (SpecialKey KeyUp) Down _ _) world =
    world { selectedAction = max 0 (selectedAction world - 1) }
handleGameInput (EventKey (SpecialKey KeyDown) Down _ _) world =
    world { selectedAction = min 2 (selectedAction world + 1) }
handleGameInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
    executeAction (selectedAction world) world
handleGameInput _ world = world

-- Ejecutar la acción seleccionada
executeAction :: Int -> GameWorld -> GameWorld
executeAction actionIndex world = 
    case actionIndex of
        0 -> world  -- Atacar (por implementar)
        1 -> world  -- Bloquear (por implementar)  
        2 -> world { currentScene = MainMenu, selectedMenuOption = 0 }  -- Escapar (volver al menú)
        _ -> world

-- Manejo de eventos en configuración/créditos
handleCreditsInput :: Event -> GameWorld -> GameWorld
handleCreditsInput (EventKey (SpecialKey KeyEsc) Down _ _) world =
    world { currentScene = MainMenu, selectedMenuOption = 0 }  -- volver con 'esc'
handleCreditsInput _ world = world

-- =============================================================================
-- ACTUALIZACIÓN DEL JUEGO
-- =============================================================================

-- Actualizar el mundo cada frame
update :: Float -> GameWorld -> GameWorld
update _ world = world  -- Sin actualizaciones automáticas por ahora

-- =============================================================================
-- MENÚ DE SELECCIÓN (consola)
-- =============================================================================

selectClass :: IO CharacterClass
selectClass = do
    putStrLn "Selecciona tu clase:"
    putStrLn "1. Warrior - Equilibrado (Vida: 100, Daño: 5.0, Velocidad: 2.0)"
    putStrLn "2. Tank - Alto HP, lento (Vida: 150, Daño: 1.5, Velocidad: 1.0)"
    putStrLn "3. Rogue - Rápido, frágil (Vida: 80, Daño: 3.5, Velocidad: 3.5)"
    putStr "Elige (1-3): "
    choice <- getLine
    case choice of
        "1" -> return Warrior
        "2" -> return Tank
        "3" -> return Rogue
        _   -> do
            putStrLn "Opción inválida. Intenta de nuevo."
            selectClass

-- =============================================================================
-- MAIN
-- =============================================================================

-- Funciones IO para playIO
renderIO :: GameWorld -> IO Picture
renderIO world = return $ render world

handleInputIO :: Event -> GameWorld -> IO GameWorld
handleInputIO event world = do
    let newWorld = handleInput event world
    if shouldExit newWorld
        then exitSuccess
        else return newWorld

updateIO :: Float -> GameWorld -> IO GameWorld
updateIO dt world = return $ update dt world

main :: IO ()
main = do
    -- Iniciar directamente en el menú gráfico
    -- Por defecto creamos un Warrior (se puede cambiar desde el menú)
    playIO window backgroundColor fps (initialWorld Warrior) renderIO handleInputIO updateIO
