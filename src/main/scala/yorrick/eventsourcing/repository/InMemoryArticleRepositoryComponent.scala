package yorrick.eventsourcing.repository


import yorrick.eventsourcing.aggregation.Article

import scala.collection.mutable


trait InMemoryArticleRepositoryComponent extends ArticleRepositoryComponent {
  class InMemoryArticleRepository extends ArticleRepository {
    private val articlesById = mutable.Map.empty[Long, Article]

    override def get(id: Long): Option[Article] = articlesById.get(id)

    override def save(article: Article): Article = {
      println(s"Saved $article")
      articlesById(article.id) = article
      article
    }
  }
}
