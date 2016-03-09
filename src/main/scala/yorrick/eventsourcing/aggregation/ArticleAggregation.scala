package yorrick.eventsourcing.aggregation

import yorrick.eventsourcing.core.EventSourcing._
import yorrick.eventsourcing.core.{EventSourcing, Update}
import yorrick.eventsourcing.repository.ArticleRepositoryComponent


case class Article(id: Long, title: String, pdfUrl: PdfUrl) extends Aggregation
case class PdfUrl(url: String, checked: Boolean)

case class SaveArticle(article: Article) extends Command
case class CheckPdfUrl(articleId: Long, pdfUrl: String) extends Command

case class ArticleCreated(article: Article) extends Event
case class ArticleUpdated(fields: Set[Update[_]]) extends Event


private object SaveArticleDiffCalculator extends DiffCalculator[SaveArticle, Article] {
  def diff(save: SaveArticle, articleOpt: Option[Article]): Option[Event] = articleOpt match {
    case Some(article) => {
      assume(save.article.id == article.id)

      val updatedFields: Set[Update[_]] = Set(
        Update.fromValues("pdfUrl", article.pdfUrl, save.article.pdfUrl),
        Update.fromValues("title", article.title, save.article.title)
      ).collect {
        case Some(u) => u
      }

      if (updatedFields.isEmpty) None else Some(ArticleUpdated(updatedFields))
    }

    case None => Some(ArticleCreated(save.article))
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

      if (updatedFields.isEmpty) None else Some(ArticleUpdated(updatedFields))
    }

    case None => throw new Exception("This should not happen")
  }
}


trait ArticleHandler extends Handler[Article] { this: ArticleRepositoryComponent =>
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

  def getDomainObject(command: Command): Option[Article] = {
    val articleId = command match {
      case c: SaveArticle => c.article.id
      case c: CheckPdfUrl => c.articleId
    }
    
    articleRepository.get(articleId)
  }

  /**
   * Proxy method to save object
   * @param article
   * @return
   */
  def saveDomainObject(article: Article): Article = articleRepository.save(article)

  /**
   * Creates new state of object using event
   * @param articleOpt
   * @param event
   * @return
   */
  def applyEvent(articleOpt: Option[Article], event: Event): Article = {
    val newArticle = event match {
      case ArticleUpdated(updates) => {
        updates.foldLeft(articleOpt.get){ case (currentArticle: Article, update) =>
          update match {
            case Update("title", _, newValue: String) => currentArticle.copy(title = newValue)
            case Update("pdfUrl", _, newValue: PdfUrl) => currentArticle.copy(pdfUrl = newValue)
            case Update("pdfUrl.checked", _, newValue: Boolean) => 
              currentArticle.copy(pdfUrl = currentArticle.pdfUrl.copy(checked = newValue))
          }
        }
        
      }
      case ArticleCreated(a) => a
    }

    newArticle
  }
}
