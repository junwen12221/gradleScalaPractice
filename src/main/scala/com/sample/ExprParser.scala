package com.sample

import scala.util.parsing.combinator._

class ExprParser extends RegexParsers {
  val number = "[0-9]+".r
  val op = "+" | "-" | "*" | "/"
  val word:Parser[String]="""[a-z]+""".r^^{_.toString};

  def expr :Parser[Int] = "(" ~ opt(op) ~ rep(term | factor) ~ ")" ^^ {
    case _ ~ _ ~ List() ~ _ => 0
    case _ ~ Some("+") ~ r ~ _ => r.reduce(_+_)
    case _ ~ Some("-") ~ r ~ _ => r.reduce(_-_)
    case _ ~ Some("*") ~ r ~ _ => r.reduce(_*_)
    case _ ~ Some("/") ~ r ~ _ => r.reduce(_/_)
  }

  def term :Parser[Int] = "(" ~ opt(op) ~ rep(factor) ~ ")" ^^ {
    case _ ~ Some("+") ~ r ~ _ => r.reduce(_+_)
    case _ ~ Some("-") ~ r ~ _ => r.reduce(_-_)
    case _ ~ Some("*") ~ r ~ _ => r.reduce(_*_)
    case _ ~ Some("/") ~ r ~ _ => r.reduce(_/_)
  }

  def factor:Parser[Int] = number ^^ {_.toInt}
}

object Scheme extends App
{
  val parser = new ExprParser
  def process():Unit = {
    val read = readLine(">>>")
    read match {
      case "exit" => ()
      case  _     =>
        val result = parser.parseAll(parser.expr, read)
        if (result.successful)
          println(result.get)
        process()
    }
  }
  println("enter exit to break")
  process()
}