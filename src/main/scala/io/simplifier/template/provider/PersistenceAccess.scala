package io.simplifier.template.provider

import io.simplifier.template.provider.PersistenceAccess.PersistenceResponse

import java.nio.charset.StandardCharsets
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait PersistenceAccess {

  def readBytes(name: String): Try[Array[Byte]]

  def readString(name: String): Try[String] = readBytes(name).map(new String(_, StandardCharsets.UTF_8))

  def readHash(name: String): Try[String]

  def writeBytes(name: String, data: Array[Byte]): Try[PersistenceResponse]

  def writeStringFuture(name: String, data: String)(implicit ec: ExecutionContext): Future[PersistenceResponse] = Future(writeString(name, data).get)

  def writeString(name: String, data: String): Try[PersistenceResponse] = writeBytes(name, data.getBytes(StandardCharsets.UTF_8))

  def readBytesFuture(name: String)(implicit ec: ExecutionContext): Future[Array[Byte]] = Future(readBytes(name).get)

  def readStringFuture(name: String)(implicit ec: ExecutionContext): Future[String] = Future(readString(name).get)

  def exists(name: String): Boolean

}

object PersistenceAccess {

  val ROOT_NAMESPACE = "/data/template"

  case class ItemNotFound(name: String) extends Throwable

  sealed trait PersistenceResponse
  case object Updated extends PersistenceResponse
  case object Linked extends PersistenceResponse
  case object Skipped extends PersistenceResponse

}
