package io.simplifier.template.provider

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import io.simplifier.template.provider.PersistenceAccess.{ItemNotFound, Linked, PersistenceResponse, Skipped, Updated}
import org.apache.commons.codec.digest.DigestUtils

import scala.util.{Failure, Success, Try}

class FileSystemAccess(namespace: String) extends PersistenceAccess {

  val absolutePathPrefix: Vector[String] = namespace.split('/').filter(_.nonEmpty).toVector

  def readBytes(name: String): Try[Array[Byte]] = {
    if (exists(name)) {
      Try {
        val link = Files.readAllBytes(absolutePath(name))
        Files.readAllBytes(Paths.get(new String(link, StandardCharsets.UTF_8)))
      }
    } else {
      Failure(ItemNotFound(name))
    }

  }

  def readHash(name: String): Try[String] = {
    Try(Files.readAllBytes(absolutePath(name)))
      .map(new String(_, StandardCharsets.UTF_8))
      .map(_.split("/").last)
  }

  def exists(name: String): Boolean = {
    Try(Files.exists(absolutePath(name))).getOrElse(false)
  }

  def absolutePath(name: String): Path = {
    val absolutePathSegments = absolutePathPrefix ++ name.split('/').filter(_.nonEmpty)
    Paths.get(absolutePathSegments.mkString("/", "/", ""))
  }

  def hash(bytes: Array[Byte]): String =
    new DigestUtils(DigestUtils.getSha256Digest).digestAsHex(bytes)

  def contentPath(hash: String): String = s"${PersistenceAccess.ROOT_NAMESPACE}/$hash"

  def existsHashFile(hash: String): Try[Boolean] = {
    Try(Files.exists(Paths.get(contentPath(hash))))
  }

  override def writeBytes(name: String, data: Array[Byte]): Try[PersistenceResponse] = {
    val contentFileName = hash(data)
    for {
      contentExists <- existsHashFile(contentFileName)
      existingLink <- Try(new String(Files.readAllBytes(absolutePath(name)), StandardCharsets.UTF_8))
      linkExists = existingLink == contentPath(contentFileName)
      response <- writeBytes(name, data, contentExists, linkExists, contentFileName)
    } yield response
  }

  def writeBytes(name: String, data: Array[Byte], contentExists: Boolean, linkExists: Boolean, hash: String): Try[PersistenceResponse] = for {
    _ <- writeContent(contentExists, data, hash)
    _ <- writeLink(linkExists, name, hash)
  } yield {
    if (linkExists && contentExists) Skipped
    else if (contentExists) Linked
    else Updated
  }

  def writeContent(contentExists: Boolean, data: Array[Byte], hash: String): Try[Unit] = {
    if (contentExists) Success(()) else {
      Try(Files.write(Paths.get(contentPath(hash)), data))
    }
  }

  def writeLink(linkExists: Boolean, name: String, hash: String): Try[Unit] = {
    if (linkExists) Success(()) else {
      Try(Files.write(absolutePath(name), contentPath(hash).getBytes(StandardCharsets.UTF_8)))
    }
  }

}

object FileSystemAccess {

  def apply(namespace: String): FileSystemAccess = new FileSystemAccess(namespace)

  def initNamespaces(namespace: String): Try[Unit] = Try {
    Files.createDirectories(Paths.get(namespace))
  }

}
