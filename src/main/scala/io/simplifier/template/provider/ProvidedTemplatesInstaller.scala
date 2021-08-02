package io.simplifier.template.provider

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.io.IOUtils

import scala.util.{Failure, Try}

object ProvidedTemplatesInstaller extends LazyLogging {

  val ROOT_NAMESPACE: String = PersistenceAccess.ROOT_NAMESPACE

  def installProvidedTemplates: Int = {
    val installedFiles = installTemplates() + installParameters()
    logger.info(s"Installed $installedFiles files.")
    installedFiles
  }

  def installTemplates(): Int = {
    val numCopied = templateFiles.filter(needToCopyTemplate).foldLeft(0){
      case (num, templateFile) =>
        (for {
          bytes <- loadTemplateResource(templateFile)
          fileName = hash(bytes)
          _ <- storeFile(fileName, bytes)
          _ <- createTemplateDirectory(templateFile)
          _ <- createTemplateLink(fileName, templateFile)
        } yield {
          logger.info(s"Copied file $templateFile")
          num + 1
        }).recoverWith {
          case error =>
            logger.error(s"Error while copying file $templateFile. Reason: ${error.getMessage}")
            Failure(error)
        }.getOrElse(num)
    }
    if (numCopied > 0) {
      logger.info(s"Copied $numCopied template files.")
    }
    numCopied
  }

  def hash(bytes: Array[Byte]): String =
    new DigestUtils(DigestUtils.getSha256Digest).digestAsHex(bytes)

  def installParameters(): Int = {
    val numCopied = parametersFiles.filter(needToCopyParameters).foldLeft(0){
      case (num, parametersFile) =>
        (for {
          bytes <- loadParametersResource(parametersFile)
          fileName = hash(bytes)
          _ <- storeFile(fileName, bytes)
          _ <- createParametersDirectory(parametersFile)
          _ <- createParametersLink(fileName, parametersFile)
        } yield {
          logger.info(s"Copied file $parametersFile")
          num + 1
        }).recoverWith {
          case error =>
            logger.error(s"Error while copying file $parametersFile. Reason: ${error.getMessage}")
            Failure(error)
        }.getOrElse(num)
    }
    if (numCopied > 0) {
      logger.info(s"Copied $numCopied parameters files.")
    }
    numCopied
  }

  def loadParametersResource(fileName: String): Try[Array[Byte]] = Try {
    val bytes = IOUtils.resourceToByteArray(parametersResourceName(fileName))
    logger.trace(s"Parameters resource $fileName loaded successfully.")
    bytes
  }

  def loadTemplateResource(fileName: String): Try[Array[Byte]] = Try {
    val bytes = IOUtils.resourceToByteArray(templateResourceName(fileName))
    logger.trace(s"Template resource $fileName loaded successfully.")
    bytes
  }

  def storeFile(name: String, bytes: Array[Byte]): Try[Unit] = Try {
    val path = Paths.get(s"$ROOT_NAMESPACE/$name")
    Files.write(path, bytes)
    logger.trace(s"File ${path.toString} stored successfully.")
  }

  def needToCopyTemplate(fileName: String): Boolean = {
    (for {
      name <- templateName(fileName)
      folder <- templateFolder(fileName)
      link = templateFileName(name, folder)
      fileName <- Try(Files.readAllBytes(Paths.get(link))).map(bytes => new String(bytes, StandardCharsets.UTF_8))
      exists <- Try(Files.exists(Paths.get(fileName)))
    } yield {
      !exists
    }).getOrElse(true)
  }

  def needToCopyParameters(fileName: String): Boolean = {
    (for {
      name <- templateName(fileName)
      folder <- templateFolder(fileName)
      link = parametersFileName(name, folder)
      fileName <- Try(Files.readAllBytes(Paths.get(link))).map(bytes => new String(bytes, StandardCharsets.UTF_8))
      exists <- Try(Files.exists(Paths.get(fileName)))
    } yield {
      !exists
    }).getOrElse(true)
  }

  def createTemplateDirectory(fileName: String): Try[Unit] =
    createDirectory(fileName, templateNamespace)

  def createParametersDirectory(fileName: String): Try[Unit] =
    createDirectory(fileName, parametersNamespace)

  def createDirectory(fileName: String, namespace: (String, String) => String): Try[Unit] =
    for {
      name <- templateName(fileName)
      folder <- templateFolder(fileName)
      _ <- Try(Files.createDirectories(Paths.get(namespace(name, folder))))
    } yield {
      logger.trace(s"Directory ${namespace(name, folder)} created successfully.")
    }

  def createTemplateLink(hash: String, fileName: String): Try[Unit] =
    createLink(hash, fileName, templateFileName)

  def createParametersLink(hash: String, fileName: String): Try[Unit] =
    createLink(hash, fileName, parametersFileName)

  def createLink(hash: String, fileName: String, linkName: (String, String) => String): Try[Unit] =
    for {
      name <- templateName(fileName)
      folder <- templateFolder(fileName)
      link = Paths.get(linkName(name, folder))
      target = s"$ROOT_NAMESPACE/$hash"
      _ <- Try(Files.write(link, target.getBytes(StandardCharsets.UTF_8)))
        .recoverWith(error => Failure(FailedLinkCreation(link.toString, target, error)))
    } yield {
      logger.trace(s"Link ${link.toString} -> $target created successfully.")
    }

  def parametersResourceName(name: String): String = {
    s"/providedTemplates/parameters/$name"
  }

  def templateResourceName(name: String): String = {
    s"/providedTemplates/templates/$name"
  }


  def templateFolder(fileName: String): Try[String] = Try {
    fileName.split('.').head.split('-').head
  }

  def templateName(fileName: String): Try[String] = Try {
    fileName.split('.').head.split('-').tail.head
  }

  def templateNamespace(name: String, folder: String): String =
    s"${TemplateProvider.NAMESPACE}/$folder/$name"

  def templateFileName(name: String, folder: String): String =
    s"${parametersNamespace(name, folder)}/${TemplateProvider.TEMPLATE_FILE_NAME}"

  def parametersNamespace(name: String, folder: String): String =
    s"${ParameterLoader.NAMESPACE}/$folder/$name"

  def parametersFileName(name: String, folder: String): String =
    s"${parametersNamespace(name, folder)}/${ParameterLoader.PARAMETERS_FILE_NAME}"

  val parametersFiles = Seq(
    "system-ForgotPasswordMail_de.json",
    "system-ForgotPasswordMail_en.json",
    "system-InviteDeveloper.json",
    "system-NewUserPasswordMail_de.json",
    "system-NewUserPasswordMail_en.json",
    "system-requestApp.json"
  )

  val templateFiles = Seq(
    "system-ForgotPasswordMail_de",
    "system-ForgotPasswordMail_en",
    "system-InviteDeveloper",
    "system-NewUserPasswordMail_de",
    "system-NewUserPasswordMail_en",
    "system-requestApp"
  )

  case class FailedLinkCreation(link: String, target: String, reason: Throwable)
    extends Throwable(s"Unable to create symbolic link $link -> $target. Reason: ${reason.getMessage} \n ${reason.getCause} \n ${reason.getStackTrace.map(_.toString).mkString("\n")}")

}
