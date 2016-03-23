package dsl.external

import scala.util.parsing.combinator.syntactical._

object OrderDSL2 extends StandardTokenParsers with App {
  val order = """(buy 100 IBM shares at max 45, sell 50 CISCO shares at min 25, 
    buy 100 Google shares at max 800) for trading account "SSS1234""""

  lexical.delimiters ++= List("(", ")", ",")
  lexical.reserved += ("buy", "sell", "shares", "at", "max", "min", "for", "trading", "account")

  case class Order(account: Account, transactions: List[Transaction])
  case class Account(number: String)
  case class Transaction(ttype: String, security: String, amount: Int, limit: Int)
  
  def instr = trans ~ account_spec ^^ { case transactions ~ account => 
    Order(account, transactions) 
  }
  def trans = "(" ~> repsep(trans_spec, ",") <~ ")"
  def trans_spec = buy_sell ~ buy_sell_instr ^^ {
    case ttype ~ (amount ~ security ~ limit) => Transaction(ttype, security, amount.toInt, limit.toInt) 
  }
  def account_spec = "for" ~> "trading" ~> "account" ~> stringLit ^^ { Account(_) }
  def buy_sell = ("buy" | "sell")
  def buy_sell_instr = security_spec ~ price_spec
  def security_spec = numericLit ~ ident <~ "shares"
  def price_spec = "at" ~> ("min" | "max") ~> numericLit
  
  def parse(input: String) = instr(new lexical.Scanner(input))

  println(parse(order))

}
