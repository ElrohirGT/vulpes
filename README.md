[![CI](https://github.com/ElrohirGT/vulpes/actions/workflows/CI.yml/badge.svg)](https://github.com/ElrohirGT/vulpes/actions/workflows/CI.yml)
[![Bundling for Various Platforms](https://github.com/ElrohirGT/vulpes/actions/workflows/Bundler.yml/badge.svg)](https://github.com/ElrohirGT/vulpes/actions/workflows/Bundler.yml)

# Vulpes

A functional programming language for making back-ends you can love!

If you know [Elm](https://elm-lang.org/) or [Roc](https://www.roc-lang.org/) then you already know Vulpes!

If you already know [Gren](https://gren-lang.org/book/)... Why are you here? Go back to the promised land.

## Simple HTTP Example

```vulpes
# Importing the Http.Server module.
# You can rename it as well as expose certain parts of it so you don't have to user the module name!
import Http.Server as HttpServer exposing (Request, Response)
import Url.Parser as P

# Type signatures are optional!
# But you can add them to improve error messages or documentation
main env flags =
  HttpServer.init {
		# If a variable has the same name as a struct field you can just write the variable name!
    env = env,
    flags, 
    init,
    onMessage
  }

init: Environment -> Flags -> Result Context DbPoolErr
init env flags =
	# Task.await will wait for the task and unwrap the result automatically!
	# If is an error then it returns the error and exits the function
  pool <- Task.await DbPool.init env.POSTGRES_URL
  logger = JsonLogger.init {
    format = Minified, 
    level = Debug
  }
  
  Context {
    pool,
    logger
  }


# This type alias is optional! In reality the compiler should be able to infer this type!
# type alias Routes = [Home, Login, User Int, NotFound]

# Creates a parser for a URL!
# Again the type signature is optional but for now we're defining them all!
# routeParser: Parser (Routes -> a) a
routeParser = 
	P.oneOf [
		P.map Home P.top,
		P.map Login (s "login"),
		P.map User (s "user" </> P.int)
	]

# Parses a given URL and converts it into a Routes tag!
# parseUrl: Url -> Routes
parseUrl url =
	let
		parsedUrl = P.parse routeParser url
	in
	match parseUrl
		Some a ->
			a

		None ->
			NotFound

# Main backend logic!
onMessage: Context -> Request -> [Response, Context]
onMessage ctx req =
  { logger, pool } = ctx
  _ <- Task.attempt
      logger.debug "Received request!" 
      |> logger.req req
      |> logger.log
	
	match parseUrl req.path
		Home ->
			Response {
				status = OK,
				body = "WELCOME TO HOME!"
			}

		Login ->
			Response {
				status = OK,
				body = "WELCOME TO LOGIN!"
			}

		User id ->
			conn <-
				pool.aquire()
				|> Task.mapErr \_ ->
					Response {
						status = SERVER_ERROR,
						body = "INTERNAL SERVER ERROR"
					}
				|> Task.await
			defer conn.release()

			Response {
				status = OK,
				body = "WELCOME TO USER!"
			}
		
		NotFound ->
			Response {
				status = NOT_FOUND,
				body = "NOT FOUND"
			}
	
```

## Roadmap

- \[ \] Lexer

- \[ \] Treesitter grammar

- \[ \] Syntax Analysis

- \[ \] Semantic Analysis

- \[ \] IR

- \[ \] Code Generator

- \[ \] LSP

- \[ \] Optimizers
