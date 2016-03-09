package yorrick.eventsourcing.repository


import yorrick.eventsourcing.aggregation.Article


trait ArticleRepositoryComponent {
  val articleRepository: ArticleRepository

  trait ArticleRepository {
    def get(id: Long): Option[Article]
    def save(article: Article): Article
  }
}
