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
    
    -- === CONFIGURACIÓN DE PISOS ===
    , getFloorData          -- Obtener datos de un piso específico
    , floorConfigurations   -- Configuración de enemigos por piso
    
    -- === FUNCIONES DEL JUEGO ===
    , doAttack              -- Función de ataque del jugador
    , takeDamage            -- Función para recibir daño
    , takeHeal              -- Función para curarse
    , powerUpDamage         -- Incrementar daño
    , powerUpSpeed          -- Incrementar velocidad
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
    , playerSpeed :: Float           -- ^ Velocidad del personaje
    } deriving (Show, Eq)


data EnemyClass = 
    Slime     -- ^ Enemigo básico: vida baja, velocidad alta
  | Reaper     -- ^ Enemigo tanque: mucha vida, lento pero fuerte
  | Skeleton  -- ^ Enemigo ágil: rápido y dañino pero frágil
  deriving (Show, Eq)

-- | Tipo de datos para enemigos
-- ESTRUCTURA SIMILAR A PLAYER para mantener consistencia
data Enemy = Enemy
    { enemyClass :: EnemyClass       -- ^ Tipo de enemigo
    , enemyHealth :: Int             -- ^ Vida actual
    , enemyDamage :: Float           -- ^ Daño base por ataque
    , enemySpeed :: Float            -- ^ Velocidad del enemigo
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
    , arenaBackground :: Picture     -- ^ Imagen de fondo de la arena (InGame)       -- ^ Sprite del enemigo
    } deriving (Show)

-- =============================================================================
-- CONSTRUCTORES
-- =============================================================================

-- Constructor de jugador según la clase elegida
createPlayer :: CharacterClass -> Player
createPlayer Warrior = Player
    { playerClass = Warrior
    , playerHealth = 100
    , playerDamage = 5.0
    , playerSpeed = 2.0
    }
createPlayer Tank = Player
    { playerClass = Tank
    , playerHealth = 150
    , playerDamage = 1.5
    , playerSpeed = 1.0
    }
createPlayer Rogue = Player
    { playerClass = Rogue
    , playerHealth = 80
    , playerDamage = 3.5
    , playerSpeed = 3.5
    }

-- Constructor de enemigo según la clase
createEnemy :: EnemyClass -> Enemy
createEnemy Slime = Enemy
    { enemyClass = Slime
    , enemyHealth = 30
    , enemyDamage = 5.0
    , enemySpeed = 8.0
    }
createEnemy Reaper = Enemy
    { enemyClass = Reaper
    , enemyHealth = 150
    , enemyDamage = 9.0
    , enemySpeed = 4.0
    }
createEnemy Skeleton = Enemy
    { enemyClass = Skeleton
    , enemyHealth = 60
    , enemyDamage = 10.0
    , enemySpeed = 12.0
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

powerUpSpeed :: Float -> State Player String
powerUpSpeed boost = do
    modify (\player -> player { playerSpeed = playerSpeed player + boost })
    return ("La velocidad ha sido incrementada en " ++ (show boost) ++ " puntos")

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
    
    -- Cargar sprites del jugador y enemigo

    
    return GameImages
        { menuBackground = menuBg
        , selectLevelBg = levelBg
        , arenaBackground = arenaBg
        }

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
        , floorEnemies = [Slime, Slime, Slime]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 3
        , floorEnemies = [Slime, Skeleton]
        , isBossFloor = False
        }
    
    -- Pisos 4-6: Zona media - Mix de enemigos
    , FloorData 
        { floorNumber = 4
        , floorEnemies = [Skeleton, Skeleton]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 5
        , floorEnemies = [Reaper]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 6
        , floorEnemies = [Skeleton, Slime, Slime]
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
        , floorEnemies = [Skeleton, Skeleton, Skeleton]
        , isBossFloor = False
        }
    , FloorData 
        { floorNumber = 9
        , floorEnemies = [Reaper, Reaper]
        , isBossFloor = False
        }
    
    -- Piso 10: BOSS FINAL
    , FloorData 
        { floorNumber = 10
        , floorEnemies = [Reaper, Skeleton, Skeleton]  -- Boss podría ser una combinación difícil
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

