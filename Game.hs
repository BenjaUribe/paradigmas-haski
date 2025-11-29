{-|
Modulo: Game
Descripcion: Modulo principal que contiene la logica del juego, tipos de datos
             y funciones para manejar jugadores, enemigos y recursos graficos.

Este modulo esta organizado en las siguientes secciones:
- TIPOS DE DATOS: Definicion de Player, Enemy, CharacterClass, etc.
- CONSTRUCTORES: Funciones para crear jugadores y enemigos
- CARGA DE RECURSOS: Funciones para cargar imagenes y recursos graficos
- LOGICA DEL JUEGO: Funciones de gameplay (combate, movimiento, etc.)

-}

module Game 
    ( -- === TIPOS DE DATOS ===
      Player(..)            -- Tipo de datos del jugador
    , Enemy(..)             -- Tipo de datos del enemigo
    , CharacterClass(..)    -- Clases de personajes (Warrior, Tank, Rogue)
    , EnemyClass(..)        -- Tipos de enemigos
    , GameImages(..)        -- Contenedor para todas las imagenes del juego
    , GameAudio(..)         -- Contenedor para toda la musica del juego
    , FloorData(..)         -- Datos de cada piso (enemigos, recompensas, etc.)
    
    -- === CONSTRUCTORES ===
    , createPlayer          -- Crear jugador con clase especifica
    , createEnemy           -- Crear enemigo con tipo especifico
    
    -- === CARGA DE RECURSOS ===
    , loadBackgroundImage   -- Cargar imagen de fondo individual
    , loadGameImages        -- Cargar todas las imagenes del juego
    , getPlayerSprite       -- Obtener sprite del jugador segun su clase
    , getEnemySprite        -- Obtener sprite del enemigo segun su clase
    , loadGameAudio         -- Cargar toda la musica del juego
    
    -- === CONFIGURACION DE PISOS ===
    , getFloorData          -- Obtener datos de un piso especifico
    , floorConfigurations   -- Configuracion de enemigos por piso
    
    -- === FUNCIONES DEL JUEGO ===
    , doAttack              -- Funcion de ataque del jugador
    , takeDamage            -- Funcion para recibir dano
    , takeHeal              -- Funcion para curarse
    , powerUpDamage         -- Incrementar dano
    ) where

-- === IMPORTS ===
import Control.Monad.State        -- Para usar la monada State en el juego
import Graphics.Gloss.Data.Picture -- Para manejar imagenes y graficos
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
  | Rogue    -- ^ Clase agil: alta velocidad y ataque, baja vida
  deriving (Show, Eq)

data Player = Player
    { playerClass :: CharacterClass  -- ^ Clase del personaje
    , playerHealth :: Int            -- ^ Vida actual
    , playerMaxHealth :: Int         -- ^ Vida maxima
    , playerDamage :: Float          -- ^ Dano base por ataque
    } deriving (Show, Eq)


data EnemyClass = 
    Slime     -- ^ Enemigo basico: vida baja, velocidad alta
  | Reaper     -- ^ Enemigo tanque: mucha vida, lento pero fuerte
  | Skeleton  -- ^ Enemigo agil: rapido y danino pero fragil
  | Warlock  -- ^ Boss
  deriving (Show, Eq)

-- | Tipo de datos para enemigos
-- ESTRUCTURA SIMILAR A PLAYER para mantener consistencia
data Enemy = Enemy
    { enemyClass :: EnemyClass       -- ^ Tipo de enemigo
    , enemyHealth :: Int             -- ^ Vida actual
    , enemyDamage :: Float           -- ^ Dano base por ataque
    } deriving (Show, Eq)

-- | Datos de configuracion para cada piso
-- Contiene informacion sobre los enemigos que aparecen en cada piso
data FloorData = FloorData
    { floorNumber :: Int             -- ^ Numero del piso (0-10)
    , floorEnemies :: [EnemyClass]   -- ^ Lista de enemigos en este piso
    , isBossFloor :: Bool            -- ^ ¿Es un piso de jefe?
    } deriving (Show, Eq)

-- | Contenedor para todas las imagenes del juego
-- PARA AGREGAR NUEVA IMAGEN:
-- 1. Agregar campo aqui
-- 2. Cargar en loadGameImages
-- 3. Usar en funciones de renderizado
data GameImages = GameImages
    { menuBackground :: Picture      -- ^ Imagen de fondo del menu principal
    , selectLevelBg :: Picture       -- ^ Imagen de fondo para seleccion de nivel
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

-- | Contenedor para toda la musica del juego
-- PARA AGREGAR NUEVA MUSICA:
-- 1. Agregar campo aqui
-- 2. Cargar en loadGameAudio
-- 3. Usar en funciones de manejo de escenas
data GameAudio = GameAudio
    { menuTheme :: FilePath          -- ^ Ruta a la musica del menu principal
    , classSelectTheme :: FilePath   -- ^ Ruta a la musica de seleccion de clase
    , levelSelectTheme :: FilePath   -- ^ Ruta a la musica de seleccion de nivel
    , battleTheme :: FilePath        -- ^ Ruta a la musica de combate
    , corridorTheme :: FilePath      -- ^ Ruta a la musica pre-batalla (Corridor)
    , bossTheme :: FilePath          -- ^ Ruta a la musica del boss final
    , victoryTheme :: FilePath       -- ^ Ruta a la musica de victoria
    , defeatTheme :: FilePath        -- ^ Ruta a la musica de derrota
    } deriving (Show)

-- =============================================================================
-- CONSTRUCTORES
-- =============================================================================

-- Constructor de jugador segun la clase elegida
createPlayer :: CharacterClass -> Player
createPlayer Warrior = Player
    { playerClass = Warrior
    , playerHealth = 100
    , playerMaxHealth = 100
    , playerDamage = 5.0
    }
createPlayer Tank = Player
    { playerClass = Tank
    , playerHealth = 150
    , playerMaxHealth = 150
    , playerDamage = 1.5
    }
createPlayer Rogue = Player
    { playerClass = Rogue
    , playerHealth = 80
    , playerMaxHealth = 80
    , playerDamage = 3.5
    }

-- Constructor de enemigo segun la clase
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
    return ("Ha recibido " ++ (show (enemyDamage enemy)) ++" de dano")

takeHeal :: Int -> State Player String
takeHeal heal = do
    modify (\player -> player {  playerHealth = playerHealth player + heal })
    return ("Ha sido curado en " ++ (show heal) ++ " puntos de vida")


powerUpDamage :: Float -> State Player String
powerUpDamage boost = do
    modify (\player -> player { playerDamage = playerDamage player + boost })
    return ("El dano ha sido incrementado en " ++ (show boost) ++ " puntos")

doAttack :: Player -> State Enemy String
doAttack player = do
    modify (\enemy -> enemy { enemyHealth = enemyHealth enemy - round (playerDamage player) })
    return ("El enemigo ha recibido " ++ (show (round (playerDamage player))) ++ " de dano")




-- =============================================================================
-- CARGA DE RECURSOS
-- =============================================================================
{-|
Esta seccion maneja la carga de todos los recursos graficos del juego.

FUNCIONES DISPONIBLES:
- loadBackgroundImage: Carga una imagen individual con manejo de errores
- loadGameImages: Carga todas las imagenes necesarias para el juego

PARA AGREGAR NUEVAS IMAGENES:
1. Colocar archivo .bmp en el directorio /img/
2. Agregar carga en loadGameImages
3. Retornar en una estructura de datos apropiada
4. Usar en las funciones de renderizado de Main.hs

FORMATOS SOPORTADOS:
- Actualmente solo BMP (por limitaciones de Gloss)
- Para otros formatos, necesitarias gloss-juicy
-}

-- | Cargar una imagen individual con manejo robusto de errores
-- Si la imagen no se puede cargar, retorna un fondo solido como respaldo
loadBackgroundImage :: String -> IO Picture
loadBackgroundImage imagePath = do
    result <- catch (loadBMP imagePath) handleError
    return result
  where
    handleError :: IOError -> IO Picture
    handleError e = do
        putStrLn $ "Warning: No se pudo cargar la imagen: " ++ imagePath
        putStrLn $ "Error: " ++ show e
        putStrLn "Usando fondo solido azul como respaldo"
        return $ color (makeColorI 25 50 100 255) $ rectangleSolid 800 600

-- | Cargar un sprite (imagen pequena) con manejo de errores
-- Si falla, retorna un placeholder mas pequeno
loadSprite :: String -> IO Picture
loadSprite imagePath = do
    result <- catch (loadBMP imagePath) handleError
    return result
  where
    handleError :: IOError -> IO Picture
    handleError e = do
        putStrLn $ "Warning: No se pudo cargar el sprite: " ++ imagePath
        putStrLn $ "Error: " ++ show e
        putStrLn "Usando circulo de placeholder"
        -- Devolver un circulo simple como placeholder
        return $ color (makeColorI 100 100 255 255) $ circleSolid 50

-- | Cargar todas las imagenes necesarias para el juego
-- Retorna un GameImages con todas las imagenes cargadas
loadGameImages :: IO GameImages
loadGameImages = do
    -- Cargar imagen de fondo del menu
    menuBg <- loadBackgroundImage "img/entry.bmp"
    
    -- Cargar imagen de fondo para seleccion de nivel
    levelBg <- loadBackgroundImage "img/niveles.bmp"
      
    -- Cargar imagen de fondo de la arena (InGame)
    arenaBg <- loadBackgroundImage "img/arena.bmp"
    
    -- Cargar sprites del jugador segun clase (usando loadSprite para mejor manejo)
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

-- | Obtener el sprite del jugador segun su clase
getPlayerSprite :: GameImages -> CharacterClass -> Picture
getPlayerSprite images Warrior = warriorSprite images
getPlayerSprite images Tank = tankSprite images
getPlayerSprite images Rogue = rogueSprite images

-- | Obtener el sprite del enemigo segun su clase
getEnemySprite :: GameImages -> EnemyClass -> Picture
getEnemySprite images Slime = slimeSprite images
getEnemySprite images Skeleton = skeletonSprite images
getEnemySprite images Reaper = reaperSprite images
getEnemySprite images Warlock = warlockSprite images

-- | Cargar todas las rutas de musica del juego
-- Retorna un GameAudio con todas las rutas a los archivos de musica
-- Nota: Las rutas se almacenan como FilePath, no se cargan directamente
-- esto permite mayor flexibilidad y control desde Main.hs
loadGameAudio :: IO GameAudio
loadGameAudio = do
    -- Verificar que existe la carpeta de audio
    putStrLn "Cargando rutas de audio..."
    
    return GameAudio
        { menuTheme = "audio/menu.wav"
        , classSelectTheme = "audio/class_select.wav"
        , levelSelectTheme = "audio/level_select.wav"
        , battleTheme = "audio/battle.wav"
        , corridorTheme = "audio/corridor.wav"
        , bossTheme = "audio/boss.wav"
        , victoryTheme = "audio/victory.wav"
        , defeatTheme = "audio/defeat.wav"
        }

-- =============================================================================
-- LOGICA DEL JUEGO
-- =============================================================================

-- =============================================================================
-- CONFIGURACION DE PISOS Y ENEMIGOS
-- =============================================================================
{-|
Esta seccion define que enemigos aparecen en cada piso del juego.

ESTRUCTURA:
- floorConfigurations: Lista con todos los pisos y sus enemigos
- getFloorData: Funcion para obtener datos de un piso especifico

PARA MODIFICAR ENEMIGOS DE UN PISO:
1. Encontrar el piso en floorConfigurations
2. Modificar la lista floorEnemies
3. Opcionalmente actualizar la descripcion

PARA AGREGAR NUEVO PISO:
1. Agregar nuevo FloorData a la lista
2. Definir los enemigos apropiados
3. Marcar isBossFloor = True si es un jefe
-}

-- | Configuracion de todos los pisos del juego
-- Define que enemigos aparecen en cada piso
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
    
    -- Pisos 7-9: Zona avanzada - Enemigos dificiles
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

-- | Obtener los datos de un piso especifico
-- Retorna Nothing si el piso no existe
getFloorData :: Int -> Maybe FloorData
getFloorData floor = 
    case filter (\fd -> floorNumber fd == floor) floorConfigurations of
        []    -> Nothing       -- Piso no encontrado
        (x:_) -> Just x        -- Retornar primer resultado (deberia ser unico)

