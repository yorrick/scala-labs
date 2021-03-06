package yorrick.eventsourcing

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import yorrick.eventsourcing.aggregation._
import yorrick.eventsourcing.core.{EventSourcing, Update}
import ArticleEventSourcing._
import EventSourcing._


class EventSourcingTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "A sequence a valid commands" should "generate a sequence of events" in {
    val article = Article(1, "Some title", PdfUrl("http://toto.com/tata.pdf", false))
    
    val cmd1 = SaveArticle(article)
    val cmd2 = SaveArticle(article.copy(title = "Some updated title"))
    val cmd3 = CheckPdfUrl(1, "http://toto.com/tata.pdf")
    val cmd4 = SaveArticle(Article(1, "Some updated title", PdfUrl("http://toto.com/titi.pdf", false)))
    val cmd5 = SaveArticle(Article(1, "Some updated title", PdfUrl("http://toto.com/titi.pdf", false)))

    val commands = Seq(cmd1, cmd2, cmd3, cmd4, cmd5)

    processCommands(None, commands: _*) shouldBe Seq(
      (Some(article), Some(ArticleCreated(article, cmd1))),
      (Some(article.copy(title = "Some updated title")), Some(ArticleUpdated(Set(Update("title", "Some title", "Some updated title")), cmd2))),
      (Some(Article(1,"Some updated title",PdfUrl("http://toto.com/tata.pdf",true))), Some(ArticleUpdated(Set(Update("pdfUrl.checked", false, true)), cmd3))),
      (Some(Article(1,"Some updated title",PdfUrl("http://toto.com/titi.pdf",false))), Some(ArticleUpdated(Set(Update("pdfUrl", PdfUrl("http://toto.com/tata.pdf", true), PdfUrl("http://toto.com/titi.pdf", false))), cmd4))),
      (Some(Article(1,"Some updated title",PdfUrl("http://toto.com/titi.pdf",false))), None)
    )
  }  
  
  "Applying event and their inverse" should "make domain objects return to their original state" in {
    val article = Article(1, "Some title", PdfUrl("http://toto.com/tata.pdf", false))
    val createEvent = ArticleCreated(article)
    val updateEvent = ArticleUpdated(Set(Update.fromValues("title", "Some title", "Some updated title").get))
    
    processEvents(Some(article), updateEvent, updateEvent.inverse).value shouldBe article
    processEvents(None, createEvent, updateEvent, updateEvent.inverse, createEvent.inverse) shouldBe None
  }
  
  "Applying some events on non existing state" should "result in failure" in {
    val updateEvent = ArticleUpdated(Set(Update.fromValues("title", "Some title", "Some updated title").get))
    
    intercept[java.lang.AssertionError] {
      processEvents(None, updateEvent)
    }
  }
}
