package yorrick.eventsourcing

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import yorrick.eventsourcing.aggregation._
import yorrick.eventsourcing.core.{Update, EventSourcing}
import EventSourcing._
import yorrick.eventsourcing.repository.InMemoryArticleRepositoryComponent


class EventSourcingTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "A sequence a valid commands" should "generate a sequence of events" in {
    implicit object Context extends ArticleHandler with InMemoryArticleRepositoryComponent {
      val articleRepository = new InMemoryArticleRepository
    }
    
    val article = Article(1, "Some title", PdfUrl("http://toto.com/tata.pdf", false))
    val commands = Seq(
      SaveArticle(article), 
      SaveArticle(article.copy(title = "Some updated title")),
      CheckPdfUrl(1, "http://toto.com/tata.pdf"),
      SaveArticle(Article(1, "Some updated title", PdfUrl("http://toto.com/titi.pdf", false))),
      SaveArticle(Article(1, "Some updated title", PdfUrl("http://toto.com/titi.pdf", false)))
    )

    process(None, commands: _*) shouldBe Seq(
      (Some(article), Some(ArticleCreated(article))),
      (Some(article.copy(title = "Some updated title")), Some(ArticleUpdated(Set(Update("title", "Some title", "Some updated title"))))),
      (Some(Article(1,"Some updated title",PdfUrl("http://toto.com/tata.pdf",true))), Some(ArticleUpdated(Set(Update("pdfUrl.checked", false, true))))),
      (Some(Article(1,"Some updated title",PdfUrl("http://toto.com/titi.pdf",false))), Some(ArticleUpdated(Set(Update("pdfUrl", PdfUrl("http://toto.com/tata.pdf", true), PdfUrl("http://toto.com/titi.pdf", false)))))),
      (Some(Article(1,"Some updated title",PdfUrl("http://toto.com/titi.pdf",false))), None)
    )
  }
}
