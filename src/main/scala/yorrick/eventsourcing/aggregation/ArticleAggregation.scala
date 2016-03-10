package yorrick.eventsourcing.aggregation

import yorrick.eventsourcing.core.EventSourcing._
import yorrick.eventsourcing.core.Update


case class Article(id: Long, title: String, pdfUrl: PdfUrl) extends Aggregation
case class PdfUrl(url: String, checked: Boolean)

case class SaveArticle(article: Article) extends Command
case class CheckPdfUrl(articleId: Long, pdfUrl: String) extends Command


case class ArticleCreated(article: Article, command: Command) extends Event {
  override def inverse: Event = ArticleDeleted(article, command.inverse)
}

case class ArticleDeleted(article: Article, command: Command) extends Event {
  override def inverse: Event = ArticleCreated(article, command.inverse)
}

case class ArticleUpdated(fields: Set[Update[_]], command: Command) extends Event {
  def inverse: ArticleUpdated = ArticleUpdated(fields.map(_.inverse), command.inverse)
}


trait DiffCalculator[-C <: Command, -A <: Aggregation] {
  def diff(command: C, existingObject: Option[A]): Option[Event]
}


private object SaveArticleDiffCalculator extends DiffCalculator[SaveArticle, Article] {
  def diff(command: SaveArticle, articleOpt: Option[Article]): Option[Event] = articleOpt match {
    case Some(article) => {
      assume(command.article.id == article.id)

      val updatedFields: Set[Update[_]] = Set(
        Update.fromValues("pdfUrl", article.pdfUrl, command.article.pdfUrl),
        Update.fromValues("title", article.title, command.article.title)
      ).collect {
        case Some(u) => u
      }

      if (updatedFields.isEmpty) None else Some(ArticleUpdated(updatedFields, command))
    }

    case None => Some(ArticleCreated(command.article, command))
  }
}


private object CheckPdfUrlDiffCalculator extends DiffCalculator[CheckPdfUrl, Article] {
  def diff(command: CheckPdfUrl, articleOpt: Option[Article]): Option[Event] = articleOpt match {
    case Some(article) => {
      assume(command.articleId == article.id)

      val updatedFields: Set[Update[_]] = Set(
        Update.fromValues("pdfUrl.checked", article.pdfUrl.checked, true)
      ).collect {
        case Some(u) => u
      }

      if (updatedFields.isEmpty) None else Some(ArticleUpdated(updatedFields, command))
    }

    case None => throw new Exception("This should not happen")
  }
}


object ArticleEventSourcing {

  implicit object ArticleHandler extends Handler[Article] {
    /**
     * Generates events from a command and an article
     * @param command
     * @param articleOpt
     * @return
     */
    def diff(command: Command, articleOpt: Option[Article]): Option[Event] = command match {
      case c: SaveArticle => SaveArticleDiffCalculator.diff(c, articleOpt)
      case c: CheckPdfUrl => CheckPdfUrlDiffCalculator.diff(c, articleOpt)
    }

    /**
     * Creates new state of object using event
     * @param articleOpt
     * @param event
     * @return
     */
    def applyEvent(articleOpt: Option[Article], event: Event): Option[Article] = {
      val newArticle: Option[Article] = event match {
        case ArticleUpdated(updates, _) => {
          val a = updates.foldLeft(articleOpt.get) { case (currentArticle: Article, update) =>
            update match {
              case Update("title", _, newValue: String) => currentArticle.copy(title = newValue)
              case Update("pdfUrl", _, newValue: PdfUrl) => currentArticle.copy(pdfUrl = newValue)
              case Update("pdfUrl.checked", _, newValue: Boolean) =>
                currentArticle.copy(pdfUrl = currentArticle.pdfUrl.copy(checked = newValue))
            }
          }

          Some(a)
        }
        case ArticleCreated(a, _) => Some(a)
        case ArticleDeleted(a, _) => None
      }

      newArticle
    }
  }
}
