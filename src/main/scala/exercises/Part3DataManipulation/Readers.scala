package exercises.Part3DataManipulation

import cats.data.Reader

object Readers extends App{

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

  case class EmailService(emailReplyTo:String)  { // who send the email?
    // to whom? and with what contents?
    def sendEmail(email:String, contents:String): String = s">>> From $emailReplyTo to $email: \n \t $contents"
  }

  // we want to create a DB connection and HttpService from the SAME configuration file;
  // <imagine loading from external location>
  val config = Configuration( // db credentials;
    dbUserName = "Robert",
    dbPassword = "password1!",
    host = "localhost",
    port= 22,
    nThreads =  12,
    emailReplyTo = "robert@mydomain.com"
  )


  // we can insert a READER to INFORM THE CREATION OF THE config to the dbConnection and HttpService
  val dbReader : Reader[Configuration, dbConnection]  = Reader(conf => dbConnection(conf.dbUserName,conf.dbPassword))
  val emailServiceReader : Reader[Configuration,EmailService] = Reader(conf => EmailService(conf.emailReplyTo))

/*
  // TODO implement email user:
  def emailUser(username:String, userEmail : String ) = {
    // 1. fetch the status of the last order
    // 2. Send email with email service to the username : "Your last order has the status $status"
  }
*/

  //------------------- SOLUTION ---------------------------------------------------------------------------------------

  def emailUser(username:String, userEmail : String ): String = {
    val emailReader : Reader[Configuration,String] = for {
      // 1. fetch the status of the last order of a given user
      orderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(orderId))
      // 2. Send email with email service to the username : "Your last order has the status $status"
      emailService  <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your last order with ID: $orderId has the status $orderStatus" )

    emailReader.run(config)
  }


  // --------------------- some tests
  println(
  emailUser("Alex","alex@myoffice.com")
  )


  // THIS LOOKS LIKE STANDARD DEPENDENCY INJECTTION;


}
