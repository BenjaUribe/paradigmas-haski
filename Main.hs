{-|
Módulo: Main
Descripción: Módulo principal que maneja la interfaz gráfica, eventos de entrada
             y el bucle principal del juego usando Gloss.

ESTRUCTURA DEL ARCHIVO:
- CONFIGURACIÓN: Ventana, colores, FPS
- TIPOS DE DATOS: GameScene, GameWorld
- RENDERIZADO: Funciones para dibujar cada escena
- MANEJO DE EVENTOS: Input de teclado y mouse
- ACTUALIZACIÓN: Lógica de update del juego
- MAIN: Punto de entrada del programa

PARA AGREGAR NUEVAS ESCENAS:
1. Añadir constructor a GameScene
2. Crear función render[NombreEscena]
3. Agregar case en render principal
4. Implementar navegación en handleInput

PARA MODIFICAR UI:
- Cambiar funciones render*
- Ajustar posiciones y tamaños en las funciones de dibujo
- Modificar colores en la sección CONFIGURACIÓN
-}

module Main where

-- === IMPORTS ===
import Game                           -- Lógica del juego y tipos de datos
import Graphics.Gloss                 -- Biblioteca gráfica principal
import Graphics.Gloss.Interface.IO.Game -- Para juegos con IO
import Graphics.Gloss.Data.Picture     -- Manejo de imágenes
import Control.Monad.State            -- Mónada State (actualmente no usada)
import qualified Data.Set as Set       -- Para conjuntos (actualmente no usado)
import System.Exit (exitSuccess)      -- Para salir del programa

-- =============================================================================
-- CONFIGURACIÓN DE LA VENTANA Y JUEGO
-- =============================================================================

-- | Dimensiones de la ventana del juego
windowWidth, windowHeight :: Float
windowWidth = 1280   -- Ancho en píxeles
windowHeight = 720   -- Alto en píxeles

-- | Configuración de la ventana del juego
-- InWindow crea una ventana redimensionable
-- Formato: InWindow "titulo" (ancho, alto) (pos_x, pos_y)
window :: Display
window = InWindow "Las flipantes aventuras del Haski-Mundo" 
                  (round windowWidth, round windowHeight) 
                  (100, 100)  -- Posición inicial en pantalla

-- | Color de fondo general del juego
-- makeColorI usa valores 0-255 para RGBA
backgroundColor :: Color
backgroundColor = makeColorI 20 20 40 255  -- Azul oscuro

-- | Frames por segundo del juego
fps :: Int
fps = 60

-- =============================================================================
-- TIPOS DE DATOS DEL JUEGO
-- =============================================================================
{-|
Esta sección define los tipos de datos específicos para la interfaz gráfica.

GAMESCENE: Representa las diferentes pantallas del juego
GAMEWORLD: Estado completo de la aplicación gráfica

PARA AGREGAR NUEVA ESCENA:
1. Añadir constructor a GameScene
2. Crear función render[NuevaEscena] en sección RENDERIZADO
3. Agregar case en función render principal
4. Implementar navegación en handleInput

PARA AGREGAR NUEVOS CAMPOS A GAMEWORLD:
1. Añadir campo aquí
2. Actualizar initialWorld
3. Usar en funciones de render y update según sea necesario
-}

-- | Escenas/pantallas disponibles en el juego
data GameScene = 
    MainMenu       -- ^ Menú principal con opciones de jugar, créditos, salir
  | ClassSelection -- ^ Pantalla de selección de clase de personaje
  | InGame         -- ^ Pantalla principal del juego (combate, HUD)
  | SelectLevel    -- ^ Pantalla de selección de nivel
  | CreditsMenu    -- ^ Pantalla de créditos
  deriving (Show, Eq)

-- | Estado completo de la aplicación gráfica
-- Este tipo contiene toda la información necesaria para renderizar y manejar eventos
data GameWorld = GameWorld
    { currentScene :: GameScene      -- ^ Escena actual que se está mostrando
    , worldPlayer :: Player          -- ^ Jugador actual (del módulo Game)
    , selectedMenuOption :: Int      -- ^ Índice de opción seleccionada en menús (0-based)
    , selectedAction :: Int          -- ^ Acción seleccionada en combate (0=Atacar, 1=Bloquear, 2=Escapar)
    , shouldExit :: Bool             -- ^ Flag para indicar que se debe cerrar el juego
    , gameImages :: GameImages       -- ^ Todas las imágenes del juego
    , accesibleLvl :: [Int]          -- ^ Niveles accesibles
    } deriving (Show)

-- Estado inicial del mundo
initialWorld :: CharacterClass -> GameImages -> GameWorld
initialWorld chosenClass images = GameWorld
    { currentScene = MainMenu
    , selectedMenuOption = 0
    , worldPlayer = createPlayer chosenClass
    , selectedAction = 0
    , shouldExit = False
    , gameImages = images
    , accesibleLvl = [1, 2, 3]  -- Inicialmente solo el primer nivel es accesible
    }

-- =============================================================================
-- RENDERIZADO
-- =============================================================================
{-|
Esta sección contiene todas las funciones de renderizado/dibujo del juego.

ORGANIZACIÓN:
- render: Función principal que delega según la escena
- render[Escena]: Una función por cada escena del juego
- render[Componente]: Funciones auxiliares para elementos de UI

PARA MODIFICAR APARIENCIA:
1. Encontrar la función render de la escena que quieres cambiar
2. Modificar posiciones usando translate
3. Cambiar colores usando color
4. Ajustar tamaños usando scale
5. Cambiar texto modificando los strings

FUNCIONES DE GLOSS ÚTILES:
- pictures [lista]: Combinar múltiples elementos
- translate x y: Mover elemento
- scale sx sy: Cambiar tamaño
- color c: Cambiar color
- text "string": Dibujar texto
- rectangleSolid w h: Rectángulo relleno
- rectangleWire w h: Rectángulo vacío
-}

-- | Función principal de renderizado
-- Delega el renderizado a la función específica de cada escena
render :: GameWorld -> Picture
render world = case currentScene world of
    MainMenu -> renderMainMenu world           -- Menú principal
    ClassSelection -> renderClassSelection world -- Selección de clase
    InGame -> renderGame world                 -- Juego principal
    SelectLevel -> renderSelectLevel world     -- Selección de nivel
    CreditsMenu -> renderCreditsMenu world     -- Créditos

-- Renderizar menú principal
renderMainMenu :: GameWorld -> Picture
renderMainMenu world = pictures
    [ -- Imagen de fondo
      menuBackground (gameImages world)
    , -- Título del juego
      translate (-600) 270 $ scale 0.5 0.5 $ color white $ text "HASKI-MUNDO"
    , -- Opciones del menú
      translate (-100) 0 $ renderMenuOptions ["Jugar", "Creditos", "Salir"] (selectedMenuOption world)
    , -- Instrucciones
      translate (-120) (-340) $ scale 0.2 0.2 $ color (greyN 0.7) $ text "Usa flechas y Enter"
    ]

-- Renderizar selección de clase
renderClassSelection :: GameWorld -> Picture
renderClassSelection world = pictures
    [ -- Fondo
      color (makeColorI 25 25 50 255) $ rectangleSolid windowWidth windowHeight
    , -- Título
      translate (-600) 250 $ scale 0.35 0.35 $ color white $ text "SELECCIONA TU CLASE"
    , -- Descripción
      translate (-500) 200 $ scale 0.2 0.2 $ color (greyN 0.8) $ text "Elige sabiamente, cada clase tiene estadísticas únicas"
    , -- Opciones de clase con estadísticas
      renderClassOptions (selectedMenuOption world)
    , -- Instrucciones
      translate (-500) (-280) $ scale 0.2 0.2 $ color (greyN 0.7) $ text "Usa flechas para navegar, Enter para confirmar, ESC para volver"
    ]

-- Renderizar las opciones de clase con sus estadísticas
renderClassOptions :: Int -> Picture
renderClassOptions selected = pictures $
    zipWith (\i (className, stats) -> 
        let yPos = 100 - fromIntegral i * 120  -- Más espacio entre clases
            isSelected = i == selected
            panelColor = if isSelected then makeColorI 60 60 100 220 else makeColorI 40 40 40 180
            borderColor = if isSelected then yellow else white
            textColor = if isSelected then yellow else white
        in pictures
            [ -- Panel de fondo para cada clase
              translate (-20) yPos $ color panelColor $ rectangleSolid 600 110
            , -- Borde del panel
              translate (-20) yPos $ color borderColor $ rectangleWire 600 110
            , -- Nombre de la clase
              translate (-250) (yPos + 15) $ scale 0.3 0.3 $ color textColor $ 
              text (if isSelected then "> " ++ className else className)
            , -- Estadísticas
              translate (-250) (yPos - 15) $ scale 0.18 0.18 $ color white $ 
              text ("Vida: " ++ show (fst4 stats) ++ "  Ataque: " ++ show (snd4 stats))
            , translate (-250) (yPos - 42) $ scale 0.18 0.18 $ color white $ 
              text ("Velocidad: " ++ show (thd4 stats) ++ "  Tipo: " ++ fth4 stats)
            ]
    ) [0..] classData
  where
    classData = [ ("WARRIOR", (100, 4.5, 2.0, "Equilibrado"))
                , ("TANK", (150, 1.5, 1.0, "Defensivo"))
                , ("ROGUE", (80, 3.0, 4.0, "Rapido"))
                ]
    fst4 (a,_,_,_) = a
    snd4 (_,b,_,_) = b
    thd4 (_,_,c,_) = c
    fth4 (_,_,_,d) = d

-- Renderizar selección de nivel
renderSelectLevel :: GameWorld -> Picture
renderSelectLevel world = pictures
    [ -- Imagen de fondo
      selectLevelBg (gameImages world)
    
    -- CORRECCIÓN AQUÍ: Agregamos (accesibleLvl world) como segundo argumento
    , renderFloorButtons (selectedMenuOption world) (accesibleLvl world)
    
    , -- Título
      translate (-600) 280 $ scale 0.4 0.4 $ color white $ text "SELECCIONA TU PISO"
    , -- Instrucciones
      translate (-400) (-320) $ scale 0.2 0.2 $ color (greyN 0.7) $ text "Usa numeros 1-9 o flechas para seleccionar, Enter para confirmar, ESC para volver"
    ]

-- Definición de las posiciones de los niveles en la pantalla
-- Formato: (NumeroDePiso, PosicionX, PosicionY)
floorPositions :: [(Int, Float, Float)]
floorPositions = 
    [ (0,   0, -265)    -- Nivel 0: Inicio (Abajo Centro)
    
    , (1, -218, -130)   -- Nivel 1: Izquierda Abajo
    , (2,    0, -130)   -- Nivel 2: Centro Abajo
    , (3,  212, -130)   -- Nivel 3: Derecha Abajo
    
    , (4, -218,    0)   -- Nivel 4: Izquierda Medio
    , (5,    0,    0)   -- Nivel 5: Centro Medio
    , (6,  212,    0)   -- Nivel 6: Derecha Medio
    
    , (7, -218, 133)   -- Nivel 7: Izquierda Arriba
    , (8,    0,  133)   -- Nivel 8: Centro Arriba
    , (9,  212,  133)   -- Nivel 9: Derecha Arriba
    
    , (10,   0,  280)   -- Nivel 10: BOSS (Arriba del todo)
    ]

-- Renderizar botones de selección de pisos
renderFloorButtons :: Int -> [Int] -> Picture
renderFloorButtons selectedFloor accessible = pictures $
    zipWith (\i floorData -> 
        let (floorNumber, xPos, yPos) = floorData
            -- Verificamos si este piso es accesible
            isAccessible = floorNumber `elem` accessible
            
            -- Lógica visual
            isSelected = i == selectedFloor
            
            -- Si es accesible usa tus colores normales, si no, usa gris oscuro (bloqueado)
            buttonColor 
                | floorNumber == 10 && isAccessible = yellow -- Boss dorado
                | not isAccessible = makeColorI 246 105 105 200
                | isSelected       = makeColorI 41 146 62 200
                | otherwise        = makeColorI 255 255 255 80
                
            borderColor = if isAccessible && isSelected then yellow else greyN 0.5
            
        in pictures
            [ translate xPos yPos $ color buttonColor $ circleSolid 25
            , translate xPos yPos $ color borderColor $ circle 25
            , translate (xPos - 10) (yPos - 8) $ scale 0.3 0.3 $ color white $ text (show floorNumber)
            -- Solo mostrar texto "PISO X" si es accesible y está seleccionado
            , if isSelected && isAccessible
                then translate (xPos + 40) yPos $ scale 0.15 0.15 $ color yellow $ text ("PISO " ++ show floorNumber)
                else blank
            ]
    ) [0..] floorPositions

-- Renderizar el juego con fondo de arena y HUD
renderGame :: GameWorld -> Picture
renderGame world = pictures
    [ -- Fondo de arena escalado y posicionado para cubrir toda la ventana
      translate 0 97 $ scale (windowWidth/800) (windowHeight/600) $ arenaBackground (gameImages world)
    , -- HUD del juego encima del fondo
      drawGameHUD world
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
          text ("Ataque: " ++ show (playerDamage player))
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
          text "Dado de Ataque: 1d8"
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

getNextLevel :: Int -> [Int]
getNextLevel currentLvl = case currentLvl of
    0 -> [1, 2, 3]   -- Inicio: se abren los 3 caminos
    -- Camino Izquierdo
    1 -> [4]         -- El 1 solo sube al 4
    4 -> [7, 8]      -- El 4 sube al 7 O cruza diagonal al 8
    7 -> [10]        -- El 7 va al boss
    -- Camino Central
    2 -> [5]         -- El 2 solo sube al 5
    5 -> [8]         -- El 5 solo sube al 8 (¿o conecta a otro lado según tu diseño?)
    8 -> [10]        -- El 8 va al boss
    -- Camino Derecho
    3 -> [6, 5]      -- El 3 sube al 6 O cruza diagonal al 5
    6 -> [9]         -- El 6 solo sube al 9
    9 -> [10]        -- El 9 va al boss
    -- Boss
    10 -> []         -- Fin del juego
    _ -> []

-- Manejar entrada del teclado según la escena
handleInput :: Event -> GameWorld -> GameWorld
handleInput event world = case currentScene world of
    MainMenu -> handleMenuInput event world
    ClassSelection -> handleClassSelectionInput event world
    InGame -> handleGameInput event world
    SelectLevel -> handleSelectLevelInput event world  
    CreditsMenu -> handleCreditsInput event world

-- Manejo de eventos en selección de nivel
handleSelectLevelInput :: Event -> GameWorld -> GameWorld
-- 1. Manejo de tecla ENTER (Seleccionar nivel)
handleSelectLevelInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
    let 
        -- El mapa ahora es directo: índice 0 es nivel 0, índice 10 es nivel 10
        selectedFloor = selectedMenuOption world
        
        -- Revisar si el piso elegido está permitido actualmente
        canEnter = selectedFloor `elem` (accesibleLvl world)
    in 
        if canEnter
        then world 
            { currentScene = InGame
            , selectedMenuOption = 0
            -- Calculamos los próximos niveles usando tu nueva lógica
            , accesibleLvl = getNextLevel selectedFloor 
            }
        else world -- Si está bloqueado, no hace nada

-- 2. Manejo de Flecha DERECHA / ARRIBA (Avanzar en la lista)
handleSelectLevelInput (EventKey (SpecialKey KeyRight) Down _ _) world =
    world { selectedMenuOption = min 10 (selectedMenuOption world + 1) }
handleSelectLevelInput (EventKey (SpecialKey KeyUp) Down _ _) world =
    world { selectedMenuOption = min 10 (selectedMenuOption world + 1) }

-- 3. Manejo de Flecha IZQUIERDA / ABAJO (Retroceder en la lista)
handleSelectLevelInput (EventKey (SpecialKey KeyLeft) Down _ _) world =
    world { selectedMenuOption = max 0 (selectedMenuOption world - 1) }
handleSelectLevelInput (EventKey (SpecialKey KeyDown) Down _ _) world =
    world { selectedMenuOption = max 0 (selectedMenuOption world - 1) }

-- 4. Manejo de ESC (Volver al menú)
handleSelectLevelInput (EventKey (SpecialKey KeyEsc) Down _ _) world =
    world { currentScene = MainMenu, selectedMenuOption = 0 }

-- 5. Ignorar cualquier otra tecla
handleSelectLevelInput _ world = world

-- Manejo de eventos en el menú principal
handleMenuInput :: Event -> GameWorld -> GameWorld
handleMenuInput (EventKey (SpecialKey KeyUp) Down _ _) world =
    world { selectedMenuOption = max 0 (selectedMenuOption world - 1) }
handleMenuInput (EventKey (SpecialKey KeyDown) Down _ _) world =
    world { selectedMenuOption = min 2 (selectedMenuOption world + 1) }
handleMenuInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
    case selectedMenuOption world of
        0 -> world { currentScene = ClassSelection, selectedMenuOption = 0 }  -- Ir a selección de clase
        1 -> world { currentScene = CreditsMenu, selectedMenuOption = 0 }  -- creditos
        2 -> world { shouldExit = True }  -- Marcar para salir
        _ -> world
handleMenuInput _ world = world

-- Manejo de eventos en selección de clase
handleClassSelectionInput :: Event -> GameWorld -> GameWorld
handleClassSelectionInput (EventKey (SpecialKey KeyEsc) Down _ _) world =
    world { currentScene = MainMenu, selectedMenuOption = 0 }
handleClassSelectionInput (EventKey (SpecialKey KeyUp) Down _ _) world =
    world { selectedMenuOption = max 0 (selectedMenuOption world - 1) }
handleClassSelectionInput (EventKey (SpecialKey KeyDown) Down _ _) world =
    world { selectedMenuOption = min 2 (selectedMenuOption world + 1) }
handleClassSelectionInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
    let chosenClass = case selectedMenuOption world of
            0 -> Warrior
            1 -> Tank
            2 -> Rogue
            _ -> Warrior
    in world { currentScene = InGame, worldPlayer = createPlayer chosenClass, selectedAction = 0 }
handleClassSelectionInput _ world = world

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
        0 -> world { currentScene = SelectLevel, selectedMenuOption = 0 }  -- Atacar -> ir a SelectLevel
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
    -- Cargar todas las imágenes del juego
    images <- loadGameImages
    
    -- Iniciar directamente en el menú gráfico
    -- Por defecto creamos un Warrior (se puede cambiar desde el menú)
    playIO window backgroundColor fps (initialWorld Warrior images) renderIO handleInputIO updateIO
