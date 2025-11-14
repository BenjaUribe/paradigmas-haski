module Game 
    ( -- Tipos de datos
      Player(..)
    , Enemy(..)
    , CharacterClass(..)
    , EnemyClass(..)
    -- , GameState(..)  -- Descomentar cuando lo definas
    -- , GameAction
    
    -- Constructores
    , createPlayer
    , createEnemy
    
    -- Funciones del juego (agregar según las vayas creando)
    -- , moverJugador
    -- , atacar
    -- , etc...
    ) where

import Control.Monad.State


-- =============================================================================
-- TIPOS DE DATOS
-- =============================================================================

-- Estado del juego
-- TODO: Define el tipo de dato que representa el estado completo del juego
-- data GameState = GameState
--   { ... campos necesarios ...
--   }


-- Otros tipos auxiliares
-- TODO: Define tipos para representar entidades del juego (jugador, enemigos, etc.)

-- clases de personajes
data CharacterClass = Warrior | Tank | Rogue deriving (Show, Eq)

data Player = Player
    {
        playerClass :: CharacterClass,
        playerHealth :: Int,
        playerDamage :: Float,
        playerSpeed :: Float
    } deriving (Show, Eq)


-- Clases de enemigos
data EnemyClass = Slime | Troll | Skeleton deriving (Show, Eq)

data Enemy = Enemy
    {
        enemyClass :: EnemyClass,
        enemyHealth :: Int,
        enemyDamage :: Float,
        enemySpeed :: Float
    } deriving (Show, Eq)



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
createEnemy Troll = Enemy
    { enemyClass = Troll
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


-- TODO: Define las acciones principales del juego usando la mónada State
-- Ejemplo:
-- moverJugador :: Direction -> GameAction ()
-- moverJugador dir = do
--   state <- get
--   put state { ... actualizar posición ... }

takeDamage :: Enemy -> State Player String
takeDamage Enemy = do
    modify (\player -> player { playerHealth = playerHealth player - round (enemyDamage Enemy) })
    return ("Ha recibido " ++ (show (enemyDamage Enemy)) ++" de daño")

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
doAtack player = do
    modify (\enemy -> enemy { enemyHealth = enemyHealth enemy - round (playerDamage player) })
    return ("El enemigo ha recibido " ++ (show (round (playerDamage player))) ++ " de daño")



-- =============================================================================
-- LÓGICA DEL JUEGO
-- =============================================================================

-- TODO: Implementa la lógica de actualización del juego

