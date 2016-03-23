package dsl.external

import scala.util.parsing.combinator.syntactical._

object OrderDSL extends StandardTokenParsers with App {
  val order = """(buy 100 IBM shares at max 45, sell 50 CISCO shares at min 25, 
    buy 100 Google shares at max 800) for trading account "SSS1234""""

  lexical.delimiters ++= List("(", ")", ",")
  lexical.reserved += ("buy", "sell", "shares", "at", "max", "min", "for", "trading", "account")

  def instr = trans ~ account_spec
  def trans = "(" ~> repsep(trans_spec, ",") <~ ")"
  def trans_spec = buy_sell ~ buy_sell_instr
  def account_spec = "for" ~> "trading" ~> "account" ~> stringLit
  def buy_sell = ("buy" | "sell")
  def buy_sell_instr = security_spec ~ price_spec
  def security_spec = numericLit ~ ident ~ "shares"
  def price_spec = "at" ~ ("min" | "max") ~ numericLit
  
  def parse(input: String) = instr(new lexical.Scanner(input))

  println(parse(order))

}

