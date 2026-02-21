#' Live Search with Debouncing
#'
#' A real-time search interface that demonstrates debounced input,
#' dynamic filtering, and highlighting search terms. Shows how to build
#' responsive UIs without lag.
#'
#' Run: Rscript inst/examples/06-live-search.R

library(alpinejs)
library(htmltools)

save_output <- interactive()

# Create the search app object with all methods
search_script <- alpine_tag("script", htmltools::HTML("
  window.searchApp = {
    searchQuery: '',
    searching: false,
    activeFilter: 'all',
    hoverId: null,
    selectedMovie: null,
    searchTimeout: null,

    movies: [
      { id: 1, title: 'Inception', year: 2010, rating: 8.8, genres: ['Sci-Fi', 'Thriller'], synopsis: 'A skilled thief who steals corporate secrets through dream-sharing technology.' },
      { id: 2, title: 'The Matrix', year: 1999, rating: 8.7, genres: ['Sci-Fi', 'Action'], synopsis: 'A computer hacker learns from mysterious rebels about the true nature of reality.' },
      { id: 3, title: 'Pulp Fiction', year: 1994, rating: 8.9, genres: ['Crime', 'Drama'], synopsis: 'The lives of two mob hitmen, a boxer, a gangster and his wife intertwine in four tales.' },
      { id: 4, title: 'The Dark Knight', year: 2008, rating: 9.0, genres: ['Action', 'Crime'], synopsis: 'Batman must battle a brilliant criminal mastermind known as the Joker.' },
      { id: 5, title: 'Parasite', year: 2019, rating: 8.6, genres: ['Thriller', 'Drama'], synopsis: 'Greed and class discrimination threaten the newly formed symbiotic relationship.' },
      { id: 6, title: 'Interstellar', year: 2014, rating: 8.6, genres: ['Sci-Fi', 'Drama'], synopsis: 'A team of explorers travel through a wormhole in space to ensure humanity survival.' },
      { id: 7, title: 'Forrest Gump', year: 1994, rating: 8.8, genres: ['Drama', 'Romance'], synopsis: 'The presidencies of Kennedy and Johnson unfold through the perspective of a simple man.' },
      { id: 8, title: 'Gladiator', year: 2000, rating: 8.5, genres: ['Action', 'Drama'], synopsis: 'A former Roman General sets out to exact vengeance against the corrupt emperor.' }
    ],

    // Get unique genres
    genres: function() {
      const allGenres = new Set();
      this.movies.forEach(m => m.genres.forEach(g => allGenres.add(g)));
      return Array.from(allGenres).sort();
    },

    // Get movies by genre
    moviesByGenre: function(genre) {
      return this.movies.filter(m => m.genres.includes(genre));
    },

    // Find results based on search and filter
    foundResults: function() {
      let results = this.movies;

      // Filter by genre
      if (this.activeFilter !== 'all') {
        results = results.filter(m => m.genres.includes(this.activeFilter));
      }

      // Filter by search query
      if (this.searchQuery) {
        const q = this.searchQuery.toLowerCase();
        results = results.filter(m =>
          m.title.toLowerCase().includes(q) ||
          m.genres.some(g => g.toLowerCase().includes(q))
        );
      }

      return results;
    },

    // Handle search with debouncing
    handleSearch: function(query) {
      this.searchQuery = query;
      
      // Clear existing timeout
      if (this.searchTimeout) {
        clearTimeout(this.searchTimeout);
      }
      
      // Set searching flag
      this.searching = true;
      
      // Debounce the search for 300ms
      this.searchTimeout = setTimeout(() => {
        this.searching = false;
      }, 300);
    },

    // Select a movie to view details
    selectMovie: function(movie) {
      this.selectedMovie = structuredClone(movie);
    }
  };
"))

app <- htmltools::tagList(
  search_script,
  alpine_tag("div",
    class = "search-app",
    style = "max-width: 700px; margin: 40px auto; font-family: sans-serif;",

    # Header
    alpine_tag("h1","Movie Search", style = "color: #333; margin-bottom: 10px;"),
    alpine_tag("p","Search our database of films (debounced input with 300ms delay)", style = "color: #666; margin-bottom: 30px;"),

    # Search container
    alpine_tag("div",
      style = "position: relative; margin-bottom: 30px;",

      # Search input
      alpine_tag("div",
        style = "position: relative;",
        alpine_tag("input",
          type = "text",
          placeholder = "Search by title or genre...",
          style = "width: 100%; padding: 15px 40px 15px 15px; font-size: 16px; border: 2px solid #ddd; border-radius: 8px; box-sizing: border-box; transition: border-color 0.3s;"
        ) |>
          alpine_on("input", "handleSearch($event.target.value)") |>
          alpine_bind("style", "{ borderColor: searching ? '#007bff' : searchQuery && !foundResults().length ? '#dc3545' : '#ddd' }"),

        # Search status indicator
        alpine_tag("div",
          style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%);",
          alpine_tag("div",
            style = "color: #007bff; font-size: 12px;"
          ) |>
            alpine_show("searching") |>
            htmltools::tagSetChildren(htmltools::HTML("Searching...")),
          alpine_tag("div",
            style = "color: #dc3545; font-size: 12px;"
          ) |>
            alpine_show("!searching && searchQuery && foundResults().length === 0") |>
            htmltools::tagSetChildren(htmltools::HTML("No results")),
          alpine_tag("div",
            style = "color: #28a745; font-size: 12px;"
          ) |>
            alpine_show("!searching && searchQuery && foundResults().length > 0") |>
            alpine_text("foundResults().length + ' found'")
        )
      ),

      # Filter tabs
      alpine_tag("div",
        style = "display: flex; gap: 10px; margin-top: 15px; flex-wrap: wrap;",

        alpine_tag("button",
          style = "border: none; padding: 8px 12px; border-radius: 4px; cursor: pointer; font-size: 13px;"
        ) |>
          alpine_text("'All (' + movies.length + ')'") |>
          alpine_bind("class", "activeFilter === 'all' ? 'filter-active' : 'filter-inactive'") |>
          alpine_on("click", "activeFilter = 'all'"),

        alpine_for(
          inner_tag = alpine_tag("button",
            style = "border: none; padding: 8px 12px; border-radius: 4px; cursor: pointer; font-size: 13px;"
          ) |>
            alpine_text("genre + ' (' + moviesByGenre(genre).length + ')'") |>
            alpine_bind("class", "activeFilter === genre ? 'filter-active' : 'filter-inactive'") |>
            alpine_on("click", "activeFilter = genre"),
          expression = "genre in genres()"
        )
      )
    ),

    # Results header
    alpine_tag("div",
      style = "margin-bottom: 15px; color: #666; font-size: 14px;"
    ) |>
      alpine_show("foundResults().length > 0") |>
      alpine_text("'Showing ' + foundResults().length + ' of ' + movies.length + ' movies'"),

    alpine_for(
      inner_tag = alpine_tag("div",
        style = "background: white; border-radius: 8px; padding: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); transition: transform 0.2s, box-shadow 0.2s; cursor: pointer; margin-bottom: 15px;",

        # Poster (placeholder)
        alpine_tag("div",
          style = "width: 100%; height: 150px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 6px; margin-bottom: 12px; display: flex; align-items: center; justify-content: center; color: white; font-size: 40px;",
          "🎬"
        ),

        # Title
        alpine_tag("div",
          style = "font-weight: bold; color: #333; margin-bottom: 6px; line-height: 1.3;"
        ) |>
          alpine_text("movie.title"),

        # Year and rating
        alpine_tag("div",
          style = "display: flex; justify-content: space-between; margin-bottom: 10px; font-size: 12px; color: #999;",
          alpine_tag("div",) |>
            alpine_text("movie.year"),
          alpine_tag("div",
            style = "color: #ffa500; font-weight: bold;"
          ) |>
            alpine_text("movie.rating + '/10'")
        ),

        # Genres
        alpine_tag("div",
          style = "display: flex; flex-wrap: wrap; gap: 6px;",
          alpine_for(
            inner_tag = alpine_tag("span",
              style = "background: #e3f2fd; color: #1976d2; padding: 2px 6px; border-radius: 3px; font-size: 11px;"
            ) |>
              alpine_text("genre"),
            expression = "genre in movie.genres"
          )
        )
      ) |>
        alpine_bind("style", "hoverId === movie.id ? 'transform: translateY(-4px); box-shadow: 0 4px 12px rgba(0,0,0,0.15);' : ''") |>
        alpine_on("mouseenter", "hoverId = movie.id") |>
        alpine_on("mouseleave", "hoverId = null") |>
        alpine_on("click", "selectMovie(movie)"),
      expression = "movie in foundResults()",
      key = "movie.id"
    ),

    # Empty state
    alpine_tag("div",
      style = "text-align: center; padding: 40px; color: #999;"
    ) |>
      alpine_show("foundResults().length === 0 && searchQuery") |>
      alpine_text("'No movies matching your search. Try different keywords.'"),

    alpine_tag("div",
      style = "text-align: center; padding: 40px; color: #999;"
    ) |>
      alpine_show("foundResults().length === 0 && !searchQuery") |>
      alpine_text("'Start typing to search our movie collection'"),

    # Movie detail modal
    alpine_tag("div",
      style = "position: fixed; inset: 0; background: rgba(0,0,0,0.5); display: flex; align-items: center; justify-content: center; z-index: 1000;",
      alpine_tag("div",
        style = "background: white; border-radius: 8px; padding: 30px; max-width: 500px; width: 90%; box-shadow: 0 10px 40px rgba(0,0,0,0.3);",

        alpine_tag("h2",
          style = "margin-top: 0; color: #333;"
        ) |>
          alpine_text("selectedMovie.title"),

        alpine_tag("div",
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-bottom: 20px;",

          alpine_tag("div",
            alpine_tag("div","Release Year", style = "color: #999; font-size: 12px;"),
            alpine_tag("div",
              style = "color: #333; font-weight: bold;"
            ) |>
              alpine_text("selectedMovie.year")
          ),

          alpine_tag("div",
            alpine_tag("div","Rating", style = "color: #999; font-size: 12px;"),
            alpine_tag("div",
              style = "color: #ffa500; font-weight: bold;"
            ) |>
              alpine_text("selectedMovie.rating + '/10'")
          )
        ),

        alpine_tag("div",
          style = "margin-bottom: 20px;",
          alpine_tag("div","Genres", style = "color: #999; font-size: 12px; margin-bottom: 8px;"),
          alpine_tag("div",
            style = "display: flex; flex-wrap: wrap; gap: 8px;",
            alpine_for(
              inner_tag = alpine_tag("span",
                style = "background: #e3f2fd; color: #1976d2; padding: 4px 8px; border-radius: 4px; font-size: 12px;"
              ) |>
                alpine_text("genre"),
              expression = "genre in selectedMovie.genres"
            )
          )
        ),

        alpine_tag("div",
          style = "margin-bottom: 20px; padding-bottom: 20px; border-bottom: 1px solid #eee;",
          alpine_tag("div","Synopsis", style = "color: #999; font-size: 12px; margin-bottom: 8px;"),
          alpine_tag("div",
            style = "color: #555; line-height: 1.6;"
          ) |>
            alpine_text("selectedMovie.synopsis")
        ),

        alpine_tag("div",
          style = "text-align: right;",
          alpine_tag("button",
            "Close",
            style = "background: #6c757d; color: white; border: none; padding: 10px 20px; border-radius: 4px; cursor: pointer; font-size: 14px;"
          ) |>
            alpine_on("click", "selectedMovie = null")
        )
      ) |>
        alpine_on("click.stop", "() => {}")
    ) |>
      alpine_show("selectedMovie") |>
      alpine_on("click", "selectedMovie = null"),

    # CSS for filter buttons
    alpine_tag("style", "
      .filter-active {
        background: #007bff !important;
        color: white !important;
      }
      .filter-inactive {
        background: #f0f0f0;
        color: #333;
      }
    ")
  ) |>
    alpine_data_js("window.searchApp")
) |>
  htmltools::browsable()

# Save to file
if (save_output) {
  htmltools::save_html(app, file = "06-live-search.html")
  cat("Saved to: 06-live-search.html\n")
}

