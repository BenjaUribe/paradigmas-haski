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
import Control.Monad.State            -- Mónada State
import Control.Monad (foldM, when)    -- Para plegar acciones monádicas y condicionales
import qualified Data.Set as Set       -- Para conjuntos (actualmente no usado)
import System.Exit (exitSuccess)      -- Para salir del programa
import System.Random (randomRIO)  -- Para generación de números aleatorios

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
  | Victory        -- ^ Pantalla de victoria
  | Defeat         -- ^ Pantalla de derrota
  | Corridor      -- ^ Pantalla intermedia antes de entrar al combate
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
    , currentFloorEnemies :: [Enemy] -- ^ Enemigos del piso actual
    , lastDiceRoll :: (Int, Int)     -- ^ Últimos valores de dados lanzados (d20, d8)
    , isBlocking :: Bool             -- ^ Flag para indicar si el jugador está bloqueando
    , combatMessage :: String        -- ^ Mensaje de combate a mostrar
    , currentFloor :: Int            -- ^ Piso actual (0-10, donde 10 es el boss final)
    , playerCorridorPos :: (Float, Float)  -- ^ Posición del jugador en pantalla Corridor (x, y)
    , keysPressed :: [Key]           -- ^ Teclas actualmente presionadas
    , blessingReached :: Bool        -- ^ Si el jugador llegó a la zona de bendición
    , selectedBlessing :: Int        -- ^ Bendición seleccionada (0=Vida, 1=Ataque)
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
    , currentFloorEnemies = []  -- Sin enemigos al inicio
    , lastDiceRoll = (0, 0)
    , isBlocking = False        -- Inicialmente no está bloqueando
    , combatMessage = ""        -- Sin mensaje inicial
    , currentFloor = 0          -- Sin piso seleccionado al inicio
    , playerCorridorPos = (0, -300)  -- Posición inicial en la parte inferior central
    , keysPressed = []          -- Sin teclas presionadas al inicio
    , blessingReached = False   -- No ha llegado a la bendición
    , selectedBlessing = 0      -- Vida por defecto
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
    Victory -> renderVictory world             -- Victoria
    Defeat -> renderDefeat world               -- Derrota
    Corridor -> renderCorridor world         -- Pantalla pre-batalla

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
              text ("Vida: " ++ show (fst3 stats) ++ "  Ataque: " ++ show (snd3 stats))
            , translate (-250) (yPos - 42) $ scale 0.18 0.18 $ color white $ 
              text ("Tipo: " ++ thd3 stats)
            ]
    ) [0..] classData
  where
    classData = [ ("WARRIOR", (100, 4.5, "Equilibrado"))
                , ("TANK", (150, 1.5, "Defensivo"))
                , ("ROGUE", (80, 3.0, "Rapido"))
                ]
    fst3 (a,_,_) = a
    snd3 (_,b,_) = b
    thd3 (_,_,c) = c

-- Renderizar selección de nivel
renderSelectLevel :: GameWorld -> Picture
renderSelectLevel world = pictures
    [ -- Imagen de fondo
      selectLevelBg (gameImages world)
    
    -- Botones de pisos
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

-- Renderizar el juego con fondo de arena, jugador, enemigos y HUD
renderGame :: GameWorld -> Picture
renderGame world = pictures
    [ -- Fondo de arena escalado y posicionado para cubrir toda la ventana
      translate 0 97 $ arenaBackground (gameImages world)
    , -- Sprite del jugador en la izquierda
      renderPlayer world
    , -- Enemigos en el área de juego (derecha)
      renderEnemies (currentFloorEnemies world) (gameImages world)
    , -- HUD del juego encima del fondo
      drawGameHUD world
    ]

-- Renderizar el sprite del jugador según su clase
renderPlayer :: GameWorld -> Picture
renderPlayer world =
    let player = worldPlayer world
        images = gameImages world
        playerSprite = getPlayerSprite images (playerClass player)
        -- Posición del jugador (izquierda de la pantalla, más arriba)
        xPos = -350
        yPos = 20
        -- Escala del sprite aumentada para mejor visibilidad
        spriteScale = 3.4
    in pictures
        [ -- Sprite del jugador escalado
          translate xPos yPos $ scale spriteScale spriteScale $ playerSprite
        , -- Vida del jugador encima del sprite
          translate (xPos - 50) (yPos + 150) $ scale 0.2 0.2 $ color yellow $
          text (show (playerHealth player) ++ " HP")
        ]

-- Renderizar enemigos con sus sprites
renderEnemies :: [Enemy] -> GameImages -> Picture
renderEnemies enemies images = pictures $ zipWith (renderEnemy images) [0..] enemies
  where
    renderEnemy :: GameImages -> Int -> Enemy -> Picture
    renderEnemy imgs index enemy =
        let -- Posiciones en horizontal, espaciados
            xPos = 200 + fromIntegral index * 180
            yPos = 0
            -- Obtener el sprite correcto según el tipo de enemigo
            enemySprite = getEnemySprite imgs (enemyClass enemy)
            -- Escala del sprite
            spriteScale = 2.9
        in pictures
            [ -- Sprite del enemigo
              translate xPos yPos $ scale spriteScale spriteScale $ enemySprite
            , -- Vida del enemigo encima
              translate (xPos - 30) (yPos + 150) $ scale 0.18 0.18 $ color red $
              text (show (enemyHealth enemy) ++ " HP")
            , -- Nombre del tipo debajo
              translate (xPos - 40) (yPos - 150) $ scale 0.15 0.15 $ color white $
              text (show (enemyClass enemy))
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

-- Renderizar pantalla de victoria
renderVictory :: GameWorld -> Picture
renderVictory world = pictures
    [ -- Fondo final
      finalBackground (gameImages world)
    , -- Texto de victoria grande
      translate (-280) 100 $ scale 0.8 0.8 $ color (makeColorI 255 215 0 255) $ text "VICTORIA"
    , -- Texto descriptivo
      translate (-500) 0 $ scale 0.3 0.3 $ color white $ text "Has derrotado a todos los enemigos"
    , -- Instrucciones
      translate (-400) (-200) $ scale 0.25 0.25 $ color (greyN 0.8) $ text "Presiona ESC para volver al menu"
    ]

-- Renderizar pantalla de derrota
renderDefeat :: GameWorld -> Picture
renderDefeat world = pictures
    [ -- Fondo final
      finalBackground (gameImages world)
    , -- Texto de derrota grande
      translate (-260) 100 $ scale 0.8 0.8 $ color red $ text "DERROTA"
    , -- Texto descriptivo
      translate (-450) 0 $ scale 0.3 0.3 $ color white $ text "Has sido derrotado en combate"
    , -- Instrucciones
      translate (-400) (-200) $ scale 0.25 0.25 $ color (greyN 0.8) $ text "Presiona ESC para volver al menu"
    ]

-- Renderizar pantalla pre-batalla
renderCorridor :: GameWorld -> Picture
renderCorridor world = 
    let (playerX, playerY) = playerCorridorPos world
        player = worldPlayer world
        images = gameImages world
        playerSprite = getPlayerSprite images (playerClass player)
        spriteScale = 2.0
        hasReachedBlessing = blessingReached world
        selectedBless = selectedBlessing world
        
        -- Verificar si el jugador está en la parte superior (y >= 200)
        canEnterCombat = playerY >= 200
    in 
        if hasReachedBlessing
        then pictures
            [ -- Fondo moai
              corridorBg images
            , -- Sprite del jugador
              translate playerX playerY $ scale spriteScale spriteScale $ playerSprite
            , -- Panel oscuro de fondo para las bendiciones
              translate 0 100 $ color (makeColorI 0 0 0 200) $ rectangleSolid 700 350
            , -- Borde del panel
              translate 0 100 $ color (makeColorI 255 215 0 255) $ rectangleWire 700 350
            , -- Mensaje de bendición
              translate (-330) 230 $ scale 0.4 0.4 $ color (makeColorI 255 215 0 255) $ text "Los Moai te bendicen!"
            , -- Subtítulo
              translate (-330) 160 $ scale 0.25 0.25 $ color white $ text "Elige tu mejora:"
            , -- Opción 0: Vida
              translate (-230) 100 $ scale 0.3 0.3 $ color (if selectedBless == 0 then yellow else white) $ 
                text (if selectedBless == 0 then "> Mejorar Vida (+20 HP)" else "  Mejorar Vida (+20 HP)")
            , -- Opción 1: Ataque
              translate (-230) 50 $ scale 0.3 0.3 $ color (if selectedBless == 1 then yellow else white) $ 
                text (if selectedBless == 1 then "> Mejorar Ataque (+5 DMG)" else "  Mejorar Ataque (+5 DMG)")
            , -- Panel oscuro para las instrucciones
              translate 0 (-250) $ color (makeColorI 0 0 0 200) $ rectangleSolid 800 50
            , -- Instrucciones
              translate (-390) (-260) $ scale 0.25 0.25 $ color green $ text "Usa flechas arriba/abajo, ENTER para confirmar"
            ]
        else pictures
            [ -- Fondo moai
              corridorBg images
            , -- Sprite del jugador
              translate playerX playerY $ scale spriteScale spriteScale $ playerSprite
            , -- Texto informativo en la parte superior
              translate (-350) 300 $ scale 0.35 0.35 $ color white $ text "Preparate para el combate"
            , -- Instrucciones dinámicas
              translate (-500) (-320) $ scale 0.25 0.25 $ color (if canEnterCombat then green else yellow) $ 
                text (if canEnterCombat
                     then "Presiona ENTER para recibir bendicion"
                     else "Mueve al personaje hacia arriba (Flechas o WASD)")
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
    , drawCombatMessage world  -- Mensaje de combate
    ]

-- Renderizar mensaje de combate en la parte superior
drawCombatMessage :: GameWorld -> Picture
drawCombatMessage world =
    if null (combatMessage world)
    then blank
    else pictures
        [ -- Fondo del mensaje
          translate 0 250 $ color (makeColorI 0 0 0 200) $ rectangleSolid 800 60
        , -- Texto del mensaje
          translate (-380) 235 $ scale 0.25 0.25 $ color red $ text (combatMessage world)
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
        ]

-- Panel de los dados
drawDicePanel :: GameWorld -> Picture
drawDicePanel world =
    let panelX = 0
        panelY = -240
        panelWidth = 300
        panelHeight = 160
        
        -- Extraemos los valores guardados
        (d20, d8) = lastDiceRoll world
        
        -- Lógica de texto: Si es 0 mostramos "-", si no, el número
        d20Text = if d20 == 0 then "-" else show d20
        d8Text  = if d8 == 0  then "-" else show d8
        
    in pictures
        [ -- Fondo del panel
          translate panelX panelY $ color (makeColorI 40 40 40 220) $ 
          rectangleSolid panelWidth panelHeight
        , -- Borde del panel
          translate panelX panelY $ color white $ 
          rectangleWire panelWidth panelHeight
        , -- Título del panel
          translate (panelX - 120) (panelY + 45) $ scale 0.18 0.18 $ color white $ 
          text "RESULTADO DADOS"
        , -- Línea separadora
          translate panelX (panelY + 35) $ color (greyN 0.6) $ 
          rectangleSolid (panelWidth - 20) 1
          
        , -- Mostramos D20 (Acierto)
          translate (panelX - 120) (panelY + 10) $ scale 0.14 0.14 $ color yellow $ 
          text ("Acierto (d20): " ++ d20Text)
          
        , -- Mostramos D8 (Daño)
          translate (panelX - 120) (panelY - 20) $ scale 0.14 0.14 $ color red $ 
          text ("Daño (d8): " ++ d8Text)
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
    5 -> [8]         -- El 5 solo sube al 8 
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
    Victory -> handleVictoryInput event world
    Defeat -> handleDefeatInput event world
    Corridor -> handleCorridorInput event world

-- Manejo de eventos en selección de nivel
handleSelectLevelInput :: Event -> GameWorld -> GameWorld
-- 1. Manejo de tecla ENTER (Seleccionar nivel)
handleSelectLevelInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
    let 
        -- El mapa ahora es directo: índice 0 es nivel 0, índice 10 es nivel 10
        selectedFloor = selectedMenuOption world
        
        -- Revisar si el piso elegido está permitido actualmente
        canEnter = selectedFloor `elem` (accesibleLvl world)
        
        -- Obtener los enemigos del piso seleccionado
        floorEnemies = case getFloorData selectedFloor of
            Nothing -> []  -- Si no hay datos, lista vacía
            Just floorData -> map createEnemy (Game.floorEnemies floorData)
        
        -- Decidir si ir a Corridor o directamente a InGame (pisos 0-3 saltan Corridor)
        nextScene = if selectedFloor <= 3 then InGame else Corridor
    in 
        if canEnter
        then world 
            { currentScene = nextScene  -- Ir a InGame si es piso 0, sino a Corridor
            , selectedMenuOption = 0
            -- Calculamos los próximos niveles usando tu nueva lógica
            , accesibleLvl = getNextLevel selectedFloor 
            -- Cargar enemigos del piso
            , currentFloorEnemies = floorEnemies
            -- Guardar el piso actual
            , currentFloor = selectedFloor
            -- Resetear posición del jugador en Corridor
            , playerCorridorPos = (0, -300)
            -- Resetear estado de bendición
            , blessingReached = False
            , selectedBlessing = 0
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
    in world 
        { currentScene = SelectLevel  -- Ir a selección de piso en lugar de InGame
        , worldPlayer = createPlayer chosenClass
        , selectedMenuOption = 0      -- Resetear selección para el mapa
        , selectedAction = 0
        }
handleClassSelectionInput _ world = world

-- Manejo de eventos en el juego (navegación de botones de acción)
handleGameInput :: Event -> GameWorld -> GameWorld
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
        0 -> world  -- Atacar (manejado por IO)
        
        1 -> performBlock world  -- Bloquear: reduce daño del siguiente turno
        2 -> performEscape world -- Escapar: intento fallido, enemigos atacan
        _ -> world

-- Intentar escapar: no se puede escapar, los enemigos atacan
performEscape :: GameWorld -> GameWorld
performEscape world =
    let worldWithMessage = world { combatMessage = "Escapar es de cobardes, seras castigado...." }
        -- Los enemigos aprovechan el intento de escape y atacan
        worldAfterAttack = enemiesAttackPlayer worldWithMessage
    in worldAfterAttack

-- Realizar bloqueo: activa el flag y los enemigos atacan con daño reducido
performBlock :: GameWorld -> GameWorld
performBlock world =
    let worldClean = world { combatMessage = "" }  -- Limpiar mensaje anterior
        worldWithBlock = worldClean { isBlocking = True }
        worldAfterEnemies = enemiesAttackPlayer worldWithBlock
    in worldAfterEnemies { isBlocking = False }  -- Desactivar bloqueo después del turno

-- Realizar un ataque al primer enemigo de la lista
-- Notar el cambio de tipo de retorno a IO GameWorld
performAttack :: Int -> Int -> GameWorld -> IO GameWorld
performAttack d20 d8 world =
    case currentFloorEnemies world of
        [] -> return world -- Si no hay enemigos, no pasa nada
        (enemy:rest) ->
            if d20 < 5 
            then do
                putStrLn "¡Has fallado tu ataque!"
                -- Limpiar mensaje y hacer que los enemigos ataquen
                let worldClean = world { combatMessage = "" }
                enemiesAttackPlayerIO worldClean
            else do
                let player = worldPlayer world
                let totalDamage = playerDamage player + fromIntegral d8
                let tempPlayer = player { playerDamage = totalDamage }

                let (message, updatedEnemy) = runState (doAttack tempPlayer) enemy
                
                let newEnemies = if enemyHealth updatedEnemy <= 0
                                then rest
                                else updatedEnemy : rest
                
                -- Limpiar mensaje al atacar
                let worldAfterPlayerAttack = world { currentFloorEnemies = newEnemies, combatMessage = "" }
                
                -- Si matamos a todos los enemigos, comprobamos victoria
                if null newEnemies
                    then return $ checkCombatStatus worldAfterPlayerAttack
                    else enemiesAttackPlayerIO worldAfterPlayerAttack

-- Los enemigos atacan al jugador en secuencia
enemiesAttackPlayer :: GameWorld -> GameWorld
enemiesAttackPlayer world =
    let enemies = currentFloorEnemies world
        player = worldPlayer world
        blocking = isBlocking world  -- Verificar si está bloqueando
        -- Aplicar el daño de todos los enemigos al jugador usando fold
        updatedPlayer = foldl (applyEnemyDamage blocking) player enemies
        worldAfterCombat = world { worldPlayer = updatedPlayer }
    in 
        -- Verificar condiciones de victoria o derrota después del combate
        checkCombatStatus worldAfterCombat
  where
    -- Función auxiliar que aplica el daño de un enemigo al jugador
    -- Si está bloqueando, reduce el daño a la mitad
    applyEnemyDamage :: Bool -> Player -> Enemy -> Player
    applyEnemyDamage blocking currentPlayer enemy =
        let reducedEnemy = if blocking 
                          then enemy { enemyDamage = enemyDamage enemy / 2 }
                          else enemy
            (message, newPlayer) = runState (takeDamage reducedEnemy) currentPlayer
        in newPlayer

-- los enemigos atacan al jugador en secuencia (versión IO con dados aleatorios)
enemiesAttackPlayerIO :: GameWorld -> IO GameWorld
enemiesAttackPlayerIO world = do
    let enemies = currentFloorEnemies world
    let player = worldPlayer world
    let blocking = isBlocking world  -- Verificar si está bloqueando
    
    -- Usamos foldM para iterar sobre los enemigos acumulando el daño en el player
    updatedPlayer <- foldM (applyEnemyTurn blocking) player enemies
    
    let worldAfterCombat = world { worldPlayer = updatedPlayer }
    return $ checkCombatStatus worldAfterCombat
  where
    -- Lógica de turno individual por enemigo
    applyEnemyTurn :: Bool -> Player -> Enemy -> IO Player
    applyEnemyTurn blocking currentPlayer enemy = do
        -- Generamos dados para ESTE enemigo
        enemyD20 <- randomRIO (0, 20) :: IO Int
        enemyD8  <- randomRIO (0, 8)  :: IO Int
        
        -- Imprimir en consola para ver qué pasa
        putStrLn $ "Enemigo " ++ show (enemyClass enemy) ++ " tira: d20=" ++ show enemyD20 ++ " d8=" ++ show enemyD8
        
        -- Lógica de acierto
        if enemyD20 < 5 
            then do
                putStrLn "¡El enemigo falló!"
                return currentPlayer
            else do
                -- Calcular daño más dado
                let baseDamage = enemyDamage enemy + fromIntegral enemyD8
                -- Si está bloqueando, reducir daño a la mitad
                let totalEnemyDamage = if blocking 
                                      then baseDamage / 2
                                      else baseDamage
                
                -- Creamos enemigo temporal para el cálculo del daño mas dado
                let tempEnemy = enemy { enemyDamage = totalEnemyDamage }
                
                -- Aplicamos el daño usando tu función pura existente
                let (_, newPlayer) = runState (takeDamage tempEnemy) currentPlayer
                
                -- Mensaje de bloqueo si corresponde
                when blocking $ putStrLn "¡Daño reducido por bloqueo!"
                
                return newPlayer

-- Verificar el estado del combate después de un turno completo
checkCombatStatus :: GameWorld -> GameWorld
checkCombatStatus world
    -- Primero verificar si no quedan enemigos
    | null (currentFloorEnemies world) = 
        -- Si estamos en el piso 10 (boss final) -> VICTORIA
        if currentFloor world == 10
        then world 
            { currentScene = Victory
            , combatMessage = ""  -- Limpiar mensaje de combate
            }
        -- Si no es el piso 10, volver a selección de nivel
        else world 
            { currentScene = SelectLevel
            , selectedMenuOption = 0
            , combatMessage = ""  -- Limpiar mensaje de combate
            }
    
    -- Luego verificar derrota: Si el jugador está muerto (HP <= 0) -> DERROTA
    | playerHealth (worldPlayer world) <= 0 = 
        world 
            { currentScene = Defeat
            , combatMessage = ""  -- Limpiar mensaje de combate
            }
    
    -- Si el jugador está vivo y quedan enemigos, continuar el combate
    | otherwise = world

-- Manejo de eventos en configuración/créditos
handleCreditsInput :: Event -> GameWorld -> GameWorld
handleCreditsInput (EventKey (SpecialKey KeyEsc) Down _ _) world =
    world { currentScene = MainMenu, selectedMenuOption = 0 }  -- volver con 'esc'
handleCreditsInput _ world = world

-- Manejo de eventos en pantalla de victoria
handleVictoryInput :: Event -> GameWorld -> GameWorld
handleVictoryInput (EventKey (SpecialKey KeyEsc) Down _ _) world =
    let restoredPlayer = createPlayer (playerClass (worldPlayer world))
    in world 
        { currentScene = MainMenu
        , selectedMenuOption = 0
        , worldPlayer = restoredPlayer  -- Restaurar vida del jugador
        , accesibleLvl = [1, 2, 3]      -- Reiniciar progreso a los 3 primeros niveles
        , currentFloorEnemies = []      -- Limpiar enemigos
        , combatMessage = ""            -- Limpiar mensaje
        , isBlocking = False            -- Resetear bloqueo
        , currentFloor = 0              -- Resetear piso actual
        }
handleVictoryInput _ world = world

-- Manejo de eventos en pantalla de derrota
handleDefeatInput :: Event -> GameWorld -> GameWorld
handleDefeatInput (EventKey (SpecialKey KeyEsc) Down _ _) world =
    let restoredPlayer = createPlayer (playerClass (worldPlayer world))
    in world 
        { currentScene = MainMenu
        , selectedMenuOption = 0
        , worldPlayer = restoredPlayer  -- Restaurar vida del jugador
        , accesibleLvl = [1, 2, 3]      -- Reiniciar progreso a los 3 primeros niveles
        , currentFloorEnemies = []      -- Limpiar enemigos
        , combatMessage = ""            -- Limpiar mensaje
        , isBlocking = False            -- Resetear bloqueo
        , currentFloor = 0              -- Resetear piso actual
        }
handleDefeatInput _ world = world

-- Manejo de eventos en pantalla pre-batalla
handleCorridorInput :: Event -> GameWorld -> GameWorld
-- ENTER: Manejar según el estado de bendición
handleCorridorInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
    let (_, playerY) = playerCorridorPos world
        hasBlessing = blessingReached world
        player = worldPlayer world
        selectedBless = selectedBlessing world
    in 
        if hasBlessing
        then 
            -- Si ya tiene bendición, aplicar mejora y comenzar combate
            let improvedPlayer = case selectedBless of
                    0 -> player { playerHealth = playerHealth player + 20 }  -- Mejorar vida
                    1 -> player { playerDamage = playerDamage player + 5 }   -- Mejorar ataque
                    _ -> player
            in world 
                { currentScene = InGame
                , worldPlayer = improvedPlayer
                , playerCorridorPos = (0, -300)
                , keysPressed = []
                , blessingReached = False  -- Resetear para próximo nivel
                , selectedBlessing = 0
                }
        else if playerY >= 200
            then world { blessingReached = True }  -- Alcanzar la bendición
            else world  -- No hacer nada si no está en posición

-- Navegación en menú de bendición
handleCorridorInput (EventKey (SpecialKey KeyUp) Down _ _) world =
    if blessingReached world
    then world { selectedBlessing = max 0 (selectedBlessing world - 1) }
    else if SpecialKey KeyUp `elem` keysPressed world
         then world
         else world { keysPressed = SpecialKey KeyUp : keysPressed world }

handleCorridorInput (EventKey (SpecialKey KeyDown) Down _ _) world =
    if blessingReached world
    then world { selectedBlessing = min 1 (selectedBlessing world + 1) }
    else if SpecialKey KeyDown `elem` keysPressed world
         then world
         else world { keysPressed = SpecialKey KeyDown : keysPressed world }

-- Registrar otras teclas presionadas (Down)
handleCorridorInput (EventKey key Down _ _) world =
    if key `elem` keysPressed world
    then world  -- Ya está registrada
    else world { keysPressed = key : keysPressed world }

-- Registrar teclas liberadas (Up)
handleCorridorInput (EventKey key Up _ _) world =
    world { keysPressed = filter (/= key) (keysPressed world) }

handleCorridorInput _ world = world

-- =============================================================================
-- ACTUALIZACIÓN DEL JUEGO
-- =============================================================================

-- Actualizar el mundo cada frame
update :: Float -> GameWorld -> GameWorld
update deltaTime world
    -- Solo actualizar movimiento en la escena Corridor
    | currentScene world == Corridor = updateCorridorMovement deltaTime world
    | otherwise = world

-- Actualizar movimiento en Corridor basado en teclas presionadas
updateCorridorMovement :: Float -> GameWorld -> GameWorld
updateCorridorMovement deltaTime world =
    -- Si ya alcanzó la bendición, no mover al jugador
    if blessingReached world
    then world
    else
        let keys = keysPressed world
            (x, y) = playerCorridorPos world
            speed = 200 * deltaTime  -- Velocidad de movimiento (píxeles por segundo)
            
            -- Límites de la zona central (más restringida)
            minX = -150
            maxX = 150
            minY = -280
            maxY = 280
            
            -- Calcular nueva posición basada en teclas presionadas
            newX = x + (if SpecialKey KeyRight `elem` keys || Char 'd' `elem` keys then speed else 0)
                     - (if SpecialKey KeyLeft `elem` keys || Char 'a' `elem` keys then speed else 0)
            newY = y + (if SpecialKey KeyUp `elem` keys || Char 'w' `elem` keys then speed else 0)
                     - (if SpecialKey KeyDown `elem` keys || Char 's' `elem` keys then speed else 0)
            
            -- Aplicar límites
            clampedX = max minX (min maxX newX)
            clampedY = max minY (min maxY newY)
        in
            world { playerCorridorPos = (clampedX, clampedY) }

-- =============================================================================
-- MAIN
-- =============================================================================

-- Funciones IO para playIO
renderIO :: GameWorld -> IO Picture
renderIO world = return $ render world

handleInputIO :: Event -> GameWorld -> IO GameWorld
handleInputIO event world = 
    case (currentScene world, selectedAction world, event) of
        (InGame, 0, EventKey (SpecialKey KeyEnter) Down _ _) -> do
            -- Generamos los números del JUGADOR
            dice20 <- randomRIO (0, 20) :: IO Int
            dice8  <- randomRIO (0, 8)  :: IO Int
            
            putStrLn $ "\n--- NUEVO TURNO ---"
            putStrLn $ "PLAYER tira: 1d20=" ++ show dice20 ++ " y 1d8=" ++ show dice8
            
            let worldWithDice = world { lastDiceRoll = (dice20, dice8) }
             
            newWorld <- performAttack dice20 dice8 worldWithDice
            
            if shouldExit newWorld then exitSuccess else return newWorld

        _ -> do
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
