<bind tag="pageTitle"><dashName /></bind>

<bind tag="pageEnd">
  <script src="/js/dashboard.js"></script>
</bind>

<apply template="base">

  <div id="carousel" class="carousel slide" data-interval="2000">
    <div class="carousel-inner">
      <graphs height="400" hideLegend="true" bgcolor="#323232" minorGridLineColor="#444444" majorGridLineColor="#404040">
        <apply template="_graph_focused" />
      </graphs>
    </div>
    <a class="left carousel-control" href="#carousel" data-slide="prev">&lsaquo;</a>
    <a class="right carousel-control" href="#carousel" data-slide="next">&rsaquo;</a>
  </div>

  <div class="container">
    <div class="row">
      <graphs width="210" height="180" hideLegend="true" hideAxes="true" bgcolor="#323232">
        <apply template="_graph_thumbnail" />
      </graphs>
    </div>
  </div>

</apply>
