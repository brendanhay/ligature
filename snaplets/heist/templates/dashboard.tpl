<bind tag="pageTitle"><dashName /></bind>

<bind tag="pageEnd">
  <script>
    !function ($) {
      $(function() {

        // Carousel
        var carousel = $('#carousel');
        carousel.carousel();

        // Thumbnail focus and tooltips
        var thumbs = $('.thumbnails a');
        thumbs.tooltip().click(function () {
          var num = $(this).data('graphNum');
          carousel.carousel(parseInt(num));
        });

        // Arrow keys
        $(document).keydown(function (e) {
           if (e.keyCode == 37) {
             carousel.carousel('prev');
           } else if (e.keyCode == 39) {
             carousel.carousel('next');
           }
        });
      });
    }(window.jQuery);
  </script>
</bind>

<apply template="base">

  <div id="carousel" class="carousel slide" data-interval="2000">
    <div class="carousel-inner">
      <graphs width="1420" height="530">
        <apply template="_graph_focused" />
      </graphs>
    </div>
    <a class="left carousel-control" href="#carousel" data-slide="prev">&lsaquo;</a>
    <a class="right carousel-control" href="#carousel" data-slide="next">&rsaquo;</a>
  </div>

  <div class="controls">
    <div class="container">
    </div>
  </div>

  <div class="container">
    <div class="thumbnails">
      <graphs width="285" height="200">
        <apply template="_graph_thumbnail" />
      </graphs>
    </div>
  </div>
</apply>
