{-|
Módulo: Game
Descripción: Módulo principal que contiene la lógica del juego, tipos de datos
             y funciones para manejar jugadores, enemigos y recursos gráficos.

Este módulo está organizado en las siguientes secciones:
- TIPOS DE DATOS: Definición de Player, Enemy, CharacterClass, etc.
- CONSTRUCTORES: Funciones para crear jugadores y enemigos
- CARGA DE RECURSOS: Funciones para cargar imágenes y recursos gráficos
- LÓGICA DEL JUEGO: Funciones de gameplay (combate, movimiento, etc.)

-}

module Game 
    ( -- === TIPOS DE DATOS ===
      Player(..)            -- Tipo de datos del jugador
    , Enemy(..)             -- Tipo de datos del enemigo
    , CharacterClass(..)    -- Clases de personajes (Warrior, Tank, Rogue)
    , EnemyClass(..)        -- Tipos de enemigos
    , GameImages(..)        -- Contenedor para todas las imágenes del juego
    , FloorData(..)         -- Datos de cada piso (enemigos, recompensas, etc.)
    
    -- === CONSTRUCTORES ===
    , createPlayer          -- Crear jugador con clase específica
    , createEnemy           -- Crear enemigo con tipo específico
    
    -- === CARGA DE RECURSOS ===
    , loadBackgroundImage   -- Cargar imagen de fondo individual
    , loadGameImages        -- Cargar todas las imágenes del juego
    , getPlayerSprite       -- Obtener sprite del jugador según su clase
    , getEnemySprite        -- Obtener sprite del enemigo según su clase
    
    -- === CONFIGURACIÓN DE PISOS ===
    , getFloorData          -- Obtener datos de un piso específico
    , floorConfigurations   -- Configuración de enemigos por piso
    
    -- === FUNCIONES DEL JUEGO ===
    , doAttack              -- Función de ataque del jugador
    , takeDamage            -- Función para recibir daño
    , takeHeal              -- Función para curarse
    , powerUpDamage         -- Incrementar daño
    ) where

-- === IMPORTS ===
import Control.Monad.State        -- Para usar la mónada State en el juego
import Graphics.Gloss.Data.Picture -- Para manejar imágenes y gráficos
import Graphics.Gloss.Data.Bitmap  -- Para cargar archivos BMP
import Graphics.Gloss.Data.Color   -- Para manejar colores
import Control.Exception (catch)   -- Para manejo de errores
import System.IO.Error (IOError)   -- Para errores de E/S


-- =============================================================================
-- TIPOS DE DATOS
-- =============================================================================

data CharacterClass = 
    Warrior  -- ^ Clase equilibrada: ataque y defensa medios
  | Tank     -- ^ Clase defensiva: alta vida, bajo ataque y velocidad
  | Rogue    -- ^ Clase ágil: alta velocidad y ataque, baja vida
  deriving (Show, Eq)

data Player = Player
    { playerClass :: CharacterClass  -- ^ Clase del personaje
    , playerHealth :: Int            -- ^ Vida actual
    , playerDamage :: Float          -- ^ Daño base por ataque
    } deriving (Show, Eq)


data EnemyClass = 
    Slime     -- ^ Enemigo básico: vida baja, velocidad alta
  | Reaper     -- ^ Enemigo tanque: mucha vida, lento pero fuerte
  | Skeleton  -- ^ Enemigo ágil: rápido y dañino pero frágil
  | Warlock  -- ^ Boss
  deriving (Show, Eq)

-- | Tipo de datos para enemigos
-- ESTRUCTURA SIMILAR A PLAYER para mantener consistencia
data Enemy = Enemy
    { enemyClass :: EnemyClass       -- ^ Tipo de enemigo
    , enemyHealth :: Int             -- ^ Vida actual
    , enemyDamage :: Float           -- ^ Daño base por ataque
    } deriving (Show, Eq)

-- | Datos de configuración para cada piso
-- Contiene información sobre los enemigos que aparecen en cada piso
data FloorData = FloorData
    { floorNumber :: Int             -- ^ Número del piso (0-10)
    , floorEnemies :: [EnemyClass]   -- ^ Lista de enemigos en este piso
    , isBossFloor :: Bool            -- ^ ¿Es un piso de jefe?
    } deriving (Show, Eq)

-- | Contenedor para todas las imágenes del juego
-- PARA AGREGAR NUEVA IMAGEN:
-- 1. Agregar campo aquí
-- 2. Cargar en loadGameImages
-- 3. Usar en funciones de renderizado
data GameImages = GameImages
    { menuBackground :: Picture      -- ^ Imagen de fondo del menú principal
    , selectLevelBg :: Picture       -- ^ Imagen de fondo para selección de nivel
    , arenaBackground :: Picture     -- ^ Imagen de fondo de la arena (InGame)
    , finalBackground :: Picture     -- ^ Imagen de fondo para victoria/derrota
    , corridorBg :: Picture         -- ^ Imagen de fondo para pantalla pre-batalla
    , warriorSprite :: Picture       -- ^ Sprite del Warrior
    , tankSprite :: Picture          -- ^ Sprite del Tank
    , rogueSprite :: Picture         -- ^ Sprite del Rogue
    , slimeSprite :: Picture         -- ^ Sprite del Slime
    , skeletonSprite :: Picture      -- ^ Sprite del Skeleton
    , reaperSprite :: Picture        -- ^ Sprite del Reaper
    , warlockSprite :: Picture       -- ^ Sprite del Warlock (Boss)
    } deriving (Show)

-- =============================================================================
-- CONSTRUCTORES
-- =============================================================================

-- Constructor de jugador según la clase elegida
createPlayer :: CharacterClass -> Player
createPlayer Warrior = Player
    { playerClass = Warrior
    , playerHealth = 1000
    , playerDamage = 5.0
    }
createPlayer Tank = Player
    { playerClass = Tank
    , playerHealth = 150
    , playerDamage = 1.5
    }
createPlayer Rogue = Player
    { playerClass = Rogue
    , playerHealth = 80
    , playerDamage = 3.5
    }

-- Constructor de enemigo según la clase
createEnemy :: EnemyClass -> Enemy
createEnemy Slime = Enemy
    { enemyClass = Slime
    , enemyHealth = 30
    , enemyDamage = 2.5
    }
createEnemy Reaper = Enemy
    { enemyClass = Reaper
    , enemyHealth = 150
    , enemyDamage = 8.5
    }
createEnemy Skeleton = Enemy
    { enemyClass = Skeleton
    , enemyHealth = 60
    , enemyDamage = 6.0
    }
createEnemy Warlock = Enemy
    { enemyClass = Warlock
    , enemyHealth = 200
    , enemyDamage = 12.0
    }


-- =============================================================================
-- ACCIONES DEL JUEGO (usando State)
-- =============================================================================
takeDamage :: Enemy -> State Player String
takeDamage enemy = do
    modify (\player -> player { playerHealth = playerHealth player - round (enemyDamage enemy) })
    return ("Ha recibido " ++ (show (enemyDamage enemy)) ++" de daño")

takeHeal :: Int -> State Player String
takeHeal heal = do
    modify (\player -> player {  playerHealth = playerHealth player + heal })
    return ("Ha sido curado en " ++ (show heal) ++ " puntos de vida")


powerUpDamage :: Float -> State Player String
powerUpDamage boost = do
    modify (\player -> player { playerDamage = playerDamage player + boost })
    return ("El daño ha sido incrementado en " ++ (show boost) ++ " puntos")

doAttack :: Player -> State Enemy String
doAttack player = do
    modify (\enemy -> enemy { enemyHealth = enemyHealth enemy - round (playerDamage player) })
    return ("El enemigo ha recibido " ++ (show (round (playerDamage player))) ++ " de daño")




-- =============================================================================
-- CARGA DE RECURSOS
-- =============================================================================
{-|
Esta sección maneja la carga de todos los recursos gráficos del juego.

FUNCIONES DISPONIBLES:
- loadBackgroundImage: Carga una imagen individual con manejo de errores
- loadGameImages: Carga todas las imágenes necesarias para el juego

PARA AGREGAR NUEVAS IMÁGENES:
1. Colocar archivo .bmp en el directorio /img/
2. Agregar carga en loadGameImages
3. Retornar en una estructura de datos apropiada
4. Usar en las funciones de renderizado de Main.hs

FORMATOS SOPORTADOS:
- Actualmente solo BMP (por limitaciones de Gloss)
- Para otros formatos, necesitarías gloss-juicy
-}

-- | Cargar una imagen individual con manejo robusto de errores
-- Si la imagen no se puede cargar, retorna un fondo sólido como respaldo
loadBackgroundImage :: String -> IO Picture
loadBackgroundImage imagePath = do
    result <- catch (loadBMP imagePath) handleError
    return result
  where
    handleError :: IOError -> IO Picture
    handleError e = do
        putStrLn $ "Warning: No se pudo cargar la imagen: " ++ imagePath
        putStrLn $ "Error: " ++ show e
        putStrLn "Usando fondo sólido azul como respaldo"
        return $ color (makeColorI 25 50 100 255) $ rectangleSolid 800 600

-- | Cargar un sprite (imagen pequeña) con manejo de errores
-- Si falla, retorna un placeholder más pequeño
loadSprite :: String -> IO Picture
loadSprite imagePath = do
    result <- catch (loadBMP imagePath) handleError
    return result
  where
    handleError :: IOError -> IO Picture
    handleError e = do
        putStrLn $ "Warning: No se pudo cargar el sprite: " ++ imagePath
        putStrLn $ "Error: " ++ show e
        putStrLn "Usando círculo de placeholder"
        -- Devolver un círculo simple como placeholder
        return $ color (makeColorI 100 100 255 255) $ circleSolid 50

-- | Cargar todas las imágenes necesarias para el juego
-- Retorna un GameImages con todas las imágenes cargadas
loadGameImages :: IO GameImages
loadGameImages = do
    -- Cargar imagen de fondo del menú
    menuBg <- loadBackgroundImage "img/entry.bmp"
    
    -- Cargar imagen de fondo para selección de nivel
    levelBg <- loadBackgroundImage "img/niveles.bmp"
      
    -- Cargar imagen de fondo de la arena (InGame)
    arenaBg <- loadBackgroundImage "img/arena.bmp"
    
    -- Cargar sprites del jugador según clase (usando loadSprite para mejor manejo)
    warriorImg <- loadSprite "img/Warrior.bmp"
    tankImg <- loadSprite "img/tank.bmp"
    rogueImg <- loadSprite "img/Rogue.bmp"
    
    -- Cargar sprites de enemigos
    slimeImg <- loadSprite "img/slime.bmp"
    skeletonImg <- loadSprite "img/skeleton.bmp"
    reaperImg <- loadSprite "img/reaper.bmp"
    warlockImg <- loadSprite "img/warlock.bmp"
    
    -- Cargar fondo final (para victoria/derrota)
    finalBg <- loadBackgroundImage "img/fondo_final.bmp"
    
    -- Cargar fondo pre-batalla
    corridorBg <- loadBackgroundImage "img/corridor.bmp"
    
    return GameImages
        { menuBackground = menuBg
        , selectLevelBg = levelBg
        , arenaBackground = arenaBg
        , finalBackground = finalBg
        , corridorBg = corridorBg
        , warriorSprite = warriorImg
        , tankSprite = tankImg
        , rogueSprite = rogueImg
        , slimeSprite = slimeImg
        , skeletonSprite = skeletonImg
        , reaperSprite = reaperImg
        , warlockSprite = warlockImg
        }

-- | Obtener el sprite del jugador según su clase
getPlayerSprite :: GameImages -> CharacterClass -> Picture
getPlayerSprite images Warrior = warriorSprite images
getPlayerSprite images Tank = tankSprite images
getPlayerSprite images Rogue = rogueSprite images

-- | Obtener el sprite del enemigo según su clase
getEnemySprite :: GameImages -> EnemyClass -> Picture
getEnemySprite images Slime = slimeSprite images
getEnemySprite images Skeleton = skeletonSprite images
getEnemySprite images Reaper = reaperSprite images
getEnemySprite images Warlock = warlockSprite images

-- =============================================================================
-- LÓGICA DEL JUEGO
-- =============================================================================

-- =============================================================================
-- CONFIGURACIÓN DE PISOS Y ENEMIGOS
-- =============================================================================
{-|
Esta sección define qué enemigos aparecen en cada piso del juego.

ESTRUCTURA:
- floorConfigurations: Lista con todos los pisos y sus enemigos
- getFloorData: Función para obtener datos de un piso específico

PARA MODIFICAR ENEMIGOS DE UN PISO:
1. Encontrar el piso en floorConfigurations
2. Modificar la lista floorEnemies
3. Opcionalmente actualizar la descripción

PARA AGREGAR NUEVO PISO:
1. Agregar nuevo FloorData a la lista
2. Definir los enemigos apropiados
3. Marcar isBossFloor = True si es un jefe
-}

-- | Configuración de todos los pisos del juego
-- Define qué enemigos aparecen en cada piso
floorConfigurations :: [FloorData]
floorConfigurations =
    [ -- Piso 0: Inicio (sin enemigos)
      FloorData 
        { floorNumber = 0
        , floorEnemies = []
        , isBossFloor = False
        }
    
    -- Pisos 1-3: Zona inicial - Slimes
    , FloorData 
        { floorNumber = 1
        , floorEnemies = [Slime, Slime]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 2
        , floorEnemies = [Slime]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 3
        , floorEnemies = [Skeleton]
        , isBossFloor = False
        }
    
    -- Pisos 4-6: Zona media - Mix de enemigos
    , FloorData 
        { floorNumber = 4
        , floorEnemies = [Skeleton, Slime]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 5
        , floorEnemies = [Reaper]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 6
        , floorEnemies = [Skeleton, Slime]
        , isBossFloor = False
        }
    
    -- Pisos 7-9: Zona avanzada - Enemigos difíciles
    , FloorData 
        { floorNumber = 7
        , floorEnemies = [Reaper, Skeleton]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 8
        , floorEnemies = [Skeleton, Slime, Skeleton]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 9
        , floorEnemies = [Reaper, Slime]
        , isBossFloor = False
        }
    
    -- Piso 10: BOSS FINAL
    , FloorData 
        { floorNumber = 10
        , floorEnemies = [Warlock] 
        , isBossFloor = True
        }
    ]

-- | Obtener los datos de un piso específico
-- Retorna Nothing si el piso no existe
getFloorData :: Int -> Maybe FloorData
getFloorData floor = 
    case filter (\fd -> floorNumber fd == floor) floorConfigurations of
        []    -> Nothing       -- Piso no encontrado
        (x:_) -> Just x        -- Retornar primer resultado (debería ser único)

