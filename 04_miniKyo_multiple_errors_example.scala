// File: 04_miniKyo_multiple_errors_example.scala

package miniKyo

// Define different error types
case class ValidationError(message: String)
case class DbError(query: String, reason: String)
case class NetworkError(url: String, statusCode: Int)

object MultipleErrorsExample:

  // --- Functions that can fail with specific error types ---

  def validateUser(name: String): String < Abort[ValidationError] =
    if name.isEmpty then Abort.fail(ValidationError("Name cannot be empty"))
    else if name.length < 3 then Abort.fail(ValidationError("Name must be at least 3 characters"))
    else name

  def saveToDb(data: String): Unit < Abort[DbError] =
    if data.contains("invalid")
    then Abort.fail(DbError(s"INSERT INTO users VALUES('$data')", "Invalid data format"))
    else ()

  def sendNotification(user: String): Unit < Abort[NetworkError] =
    if user == "unreachable"
    then Abort.fail(NetworkError("https://api.example.com/notify", 503))
    else ()

  // --- Composing multiple error types ---

  def registerUser(name: String): Unit < (Abort[ValidationError] & Abort[DbError] & Abort[NetworkError]) =
    for
      validName <- validateUser(name)
      _         <- saveToDb(validName)
      _         <- sendNotification(validName)
    yield ()

  // --- Handling errors independently ---

  def handleValidation[A, S](v: A < (S & Abort[ValidationError])): Result[ValidationError, A] < S =
    Abort.run(v)

  def handleDb[A, S](v: A < (S & Abort[DbError])): Result[DbError, A] < S =
    Abort.run(v)

  def handleNetwork[A, S](v: A < (S & Abort[NetworkError])): Result[NetworkError, A] < S =
    Abort.run(v)

  // --- Converting specific errors to general ones ---

  def toGeneralError[A, S](v: A < (S & Abort[ValidationError] & Abort[DbError] & Abort[NetworkError])): A < (S & Abort[String]) =
    // Handle ValidationError and convert to String
    val result1: Result[ValidationError, A] < (S & Abort[DbError] & Abort[NetworkError]) = Abort.run(v)
    val withValidation: A < (S & Abort[DbError] & Abort[NetworkError] & Abort[String])   =
      result1.flatMap:
        case Result.Success(a) => a
        case Result.Failure(e) => Abort.fail(s"Validation failed: ${e.message}")
        case Result.Panic(t)   => Abort.panic(t)

    // Handle DbError and convert to String
    val result2: Result[DbError, A] < (S & Abort[NetworkError] & Abort[String]) = Abort.run(withValidation)
    val withDb: A < (S & Abort[NetworkError] & Abort[String])                   =
      result2.flatMap:
        case Result.Success(a) => a
        case Result.Failure(e) => Abort.fail(s"Database error: ${e.reason} (query: ${e.query})")
        case Result.Panic(t)   => Abort.panic(t)

    // Handle NetworkError and convert to String
    val result3: Result[NetworkError, A] < (S & Abort[String]) = Abort.run(withDb)
    val withNetwork: A < (S & Abort[String])                   =
      result3.flatMap:
        case Result.Success(a) => a
        case Result.Failure(e) => Abort.fail(s"Network error: ${e.url} returned ${e.statusCode}")
        case Result.Panic(t)   => Abort.panic(t)

    withNetwork

  // --- Example: Handling errors with fallbacks ---

  def registerUserWithFallback(name: String): String < Sync =
    val program: Unit < (Abort[ValidationError] & Abort[DbError] & Abort[NetworkError]) = registerUser(name)

    // Handle validation errors first
    val step1: Result[ValidationError, Unit] < (Abort[DbError] & Abort[NetworkError] & Sync) =
      Abort.run(program)

    val withValidationHandling: Unit < (Abort[DbError] & Abort[NetworkError] & Sync) =
      step1.flatMap:
        case Result.Success(_) => Kyo.unit
        case Result.Failure(e) => Console.printline(s"❌ Validation error: ${e.message}")
        case Result.Panic(t)   => Abort.panic(t)

    // Handle database errors
    val step2: Result[DbError, Unit] < (Abort[NetworkError] & Sync) =
      Abort.run(withValidationHandling)

    val withDbHandling: Unit < (Abort[NetworkError] & Sync) =
      step2.flatMap:
        case Result.Success(_) => Kyo.unit
        case Result.Failure(e) => Console.printline(s"❌ Database error: ${e.reason}")
        case Result.Panic(t)   => Abort.panic(t)

    // Handle network errors
    val step3: Result[NetworkError, Unit] < Sync =
      Abort.run(withDbHandling)

    val withNetworkHandling: Unit < Sync =
      step3.flatMap:
        case Result.Success(_) => Kyo.unit
        case Result.Failure(e) => Console.printline(s"⚠️  Network error (${e.statusCode}), user saved but notification failed")
        case Result.Panic(t)   => Abort.panic(t)

    withNetworkHandling *> "Registration process completed"

  // --- Example: Partial recovery (continue on network errors) ---

  def registerUserPartialRecovery(name: String): String < (Abort[ValidationError] & Abort[DbError] & Sync) =
    val program: Unit < (Abort[ValidationError] & Abort[DbError] & Abort[NetworkError]) = registerUser(name)

    // Only handle network errors - let validation and DB errors propagate
    val networkResult: Result[NetworkError, Unit] < (Abort[ValidationError] & Abort[DbError] & Sync) =
      Abort.run(program)

    val withNetworkRecovery: Unit < (Abort[ValidationError] & Abort[DbError] & Sync) =
      networkResult.flatMap:
        case Result.Success(_) => Kyo.unit
        case Result.Failure(e) => Console.printline(s"⚠️  Network error (${e.statusCode}), but user was saved to DB")
        case Result.Panic(t)   => Abort.panic(t)

    withNetworkRecovery *> "User registered (notifications might have failed)"

end MultipleErrorsExample

// --- Runnable demo ---

object MultipleErrorsDemo extends KyoApp:
  import MultipleErrorsExample.*

  run:
    Console.printline("=== Multiple Error Types Demo ===") *>
      Console.printline("") *>
      Console.printline("1. Valid user:") *>
      registerUserWithFallback("Alice") *>
      Console.printline("") *>
      Console.printline("2. Validation error (empty name):") *>
      registerUserWithFallback("") *>
      Console.printline("") *>
      Console.printline("3. Validation error (too short):") *>
      registerUserWithFallback("ab") *>
      Console.printline("") *>
      Console.printline("4. Database error:") *>
      registerUserWithFallback("invalid") *>
      Console.printline("") *>
      Console.printline("5. Network error:") *>
      registerUserWithFallback("unreachable")
