(function($) {

    var carousel = $('#carousel');
    carousel.carousel();

    var graphs = $('.graph a');
    graphs.tooltip().click(function() {
        var num = $(this).data('graphNum');
        carousel.carousel(parseInt(num));
    });

    $(document).keydown(function(e) {
        if (e.keyCode == 37) {
            carousel.carousel('prev');
        } else if (e.keyCode == 39) {
            carousel.carousel('next');
        }
    });


    var images = $('img.graph-image').hide();
    function resized() {
        var width = $(window).width();

        images.each(function(i, elem) {
            var img = $(elem);
            var src = img.data('src');

            if (img.data('resize')) {
                img.css('width', width + 'px');
                img.attr('src', src + '&width=' + width);
            } else {
                img.attr('src', src);
            }
        }).show();
    }

    resized();

    var refreshEvery = 30000,
        resizeAfter  = 150;

    var resizeTimer;
    var refreshTimer = setTimeout(resized, refreshEvery);

    $(window).resize(function() {
        clearTimeout(resizeTimer);
        clearTimeout(refreshTimer);

        resizeTimer  = setTimeout(resized, 150);
        refreshTimer = setTimeout(resized, refreshEvery);
    });

})(window.jQuery);
