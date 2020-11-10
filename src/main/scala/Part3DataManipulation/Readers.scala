package Part3DataManipulation

object Readers extends App{

  /* Multi-layers application, with :
   - configuration file -> internal data structure
   - Database Layer
   - HTTP Layer
   - Business logic

   All subsequent layers ARE INFORMED about the initial configuration file fetched from somewhere;

  The idea is to feed this initial config file into the entire application; The reader data type embodies this principle;

  */


  case class Configuration(dbUserName:String, dbPassword:String, host: String, port:Int, nThreads:Int, emailReplyTo: String)

  // LAYER #1:  dbConnection layer (a simplification)
  case class dbConnection(userName:String, password:String) {
    def getOrderStatus(orderId:Long):String = "dispatched" // select <something> from db_table, and returns the status of order ID
    // above implementation is not important;

    def getLastOrderId(username:String): Long = 51245 // some random number, in reality something like:
    // select max(orderId) from table where username=username (pseudo-SQL)
  }


  // LAYER #2: HTTP layer - simplification
  case class HttpService(host:String, port:Int){
    def start(): Unit = println("server started") // opens the port, and starts the server for incoming connections "in real life"
  }

  // we want to create a DB connection and HttpService from the SAME configuration file;
  // <imagine loading from external location>
  val config = Configuration(
    dbUserName = "Robert",
    dbPassword = "password1!",
    host = "localhost",
    port= 22,
    nThreads =  12,
    emailReplyTo = "robert@mydomain.com"
  )


  // we can insert a READER to INFORM THE CREATION OF THE config to the dbConnection and HttpService

  //cats Reader
  import cats.data.Reader

  // reader has TWO type arguments, first Input type, and second - the output type
  val dbReader : Reader[Configuration, dbConnection]  = Reader(conf => dbConnection(conf.dbUserName,conf.dbPassword))
  // TYPE PARAMETERS -> reads from 'Configuration', and inform the creation of a 'dbConnection'
  // the 'Reader()' apply factory method takes a function from 'Configuration' to the 'dbConnection'
  // basically a Reader is a specification of how to derive a 'dbConnection' from a provided 'Configuration'

  // how to use it? we want to get the output 'dbConnection' from a given 'config'
  val dbConn = dbReader.run(config)

  // the neat thing is that we can use 'map' on the return type!
  val someOrderId = 55L
  val myOrderStatusREADER : Reader[Configuration, String] = dbReader.map(someConnection => someConnection.getOrderStatus(someOrderId))
  println(myOrderStatusREADER.run(config))
  // maps the output of a dbReader ('dbConnection')
  // the input 'config' remains the same, plus there is an additional `someOrderId`


  def getLastOrderStatus(username:String ): String = {

    // for a given username, return his last order ID
    val usersLastOrderStatusREADER : Reader[Configuration,String] = dbReader // chain of operations
      .map(_.getLastOrderId(username=username)) // dbReader returns connection,
      // and this connection is mapped with method argument `username`, to return some order ID
      .flatMap(orderId => dbReader.map(_.getOrderStatus(orderId)) ) // this order is flatMapped to RUN ANOTHER READER returning a connection

    usersLastOrderStatusREADER.run(config)
  }

  // the above is a nice chain of commands, however it can be prettier
  // as map and flatMap can be replaced with for syntax:

  def getLastOrderStatusBetter(username:String): String = {
    val usersLastOrderStatusReader : Reader[Configuration,String] = for {
      orderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(orderId))
    } yield orderStatus

    usersLastOrderStatusReader.run(config) // Config -> Connection -> gets last order ID for a user -> Connection -> gets status for that order ID
  }


  println(getLastOrderStatus("Robert")) // Returns: "dispatched"
  //====================================================================================================================
  /*
  THE GENERAL PATTERN:
  1. Create initial data structure
  2. Create a READER which specifies how that data structure will be manipulated later
  3. Can map/flatMap the reader to produce derived information
  4. The `.run()` method obtains the final/requested piece of information, when provided with the initial data structure;




   */


}
