def getTransactions(): Array[Array[String]] = {

  val rows = 5
  val cols = 4

  // Declaring Multidimension array
  val transactions = Array.ofDim[String](rows, cols)

  // Allocating values
  transactions(0)(0) = "1" //transaction_id
  transactions(0)(1) = "3545" //client_id
  transactions(0)(2) = "3000" //total_amount
  transactions(0)(3) = "6.99" //discount_percentage

  transactions(1)(0) = "2" //transaction_id
  transactions(1)(1) = "3545" //client_id
  transactions(1)(2) = "4500" //total_amount
  transactions(1)(3) = "0.45" //discount_percentage

  transactions(2)(0) = "3" //transaction_id
  transactions(2)(1) = "3509" //client_id
  transactions(2)(2) = "69998" //total_amount
  transactions(2)(3) = "0" //discount_percentage

  transactions(3)(0) = "4" //transaction_id
  transactions(3)(1) = "3510" //client_id
  transactions(3)(2) = "1" //total_amount
  transactions(3)(3) = "null" //discount_percentage

  transactions(4)(0) = "5" //transaction_id
  transactions(4)(1) = "4510" //client_id
  transactions(4)(2) = "34" //total_amount
  transactions(4)(3) = "40" //discount_percentage
  
  println("Creating transaction dataset:")
  transactions.map(t => println(s"${t(0)}, ${t(1)}, ${t(2)}, ${t(3)}"))
  println("")

  return transactions

}

def getContracts(): Array[Array[String]] = {

  val rows = 4
  val cols = 5

  // Declaring Multidimension array
  val contracts = Array.ofDim[String](rows, cols)

  // Allocating values
  contracts(0)(0) = "3" //contract_id
  contracts(0)(1) = "3545" //client_id
  contracts(0)(2) = "Magazine Luana" //client_name
  contracts(0)(3) = "2.00" //percentage
  contracts(0)(4) = "true" //is_active

  contracts(1)(0) = "4" //contract_id
  contracts(1)(1) = "3545" //client_id
  contracts(1)(2) = "Magazine Luana" //client_name
  contracts(1)(3) = "1.95" //percentage
  contracts(1)(4) = "false" //is_active

  contracts(2)(0) = "5" //contract_id
  contracts(2)(1) = "3509" //client_id
  contracts(2)(2) = "Lojas Italianas" //client_name
  contracts(2)(3) = "1" //percentage
  contracts(2)(4) = "true" //is_active

  contracts(3)(0) = "6" //contract_id
  contracts(3)(1) = "3510" //client_id
  contracts(3)(2) = "Carrerfive" //client_name
  contracts(3)(3) = "3.00" //percentage
  contracts(3)(4) = "true" //is_active
  
  println("Creating contracts dataset:")
  contracts.map(t => println(s"${t(0)}, ${t(1)}, ${t(2)}, ${t(3)}, ${t(4)}"))
  println("")

  return contracts

}

object getTotalGain {
  var transactions = getTransactions()
  var contracts = getContracts()
  var totalGain = BigDecimal("0")

  //filters all active contracts
  contracts.filter(c => c(4) == "true")
           .map(c =>
                transactions.filter(t => t(1) == c(1))
                            .map(t => 
                                 //println(s"${t(0)}, ${t(1)}, ${c(3)}")
                                 //converts null values to zero
                                 val discount_perc = if (t(3) == "null") { BigDecimal(0) }
                                                     else { BigDecimal(t(3)) }
                                //apply discount for each transaction
                                 val total_amount_with_discount =
                                 BigDecimal(t(2)) * (1 - discount_perc / 100)
                                 
                                 //calculate profit per transaction
                                 val gain = total_amount_with_discount * (BigDecimal(c(3)) / 100)
                                 
                                 totalGain += gain
                                 
                                 println(s"Transaction_ID: ${t(0)}, Contract_ID: ${c(0)}, ${c(2)}, had a gain: ${gain}")
                                 
                                )
               )
  
  println("")
  println(s"The total gain is ${totalGain}")
  
  println("")
  println("Challenge for data engineer")
  println("Developed by Marcelo Abdiel")

}

getTotalGain
