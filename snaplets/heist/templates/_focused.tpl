<div class="item ${graphActive}" style="height: ${height}px">
  <div class="container">
    <div class="carousel-caption">
      <h1><graphName /></h1>
      <p class="lead"><graphDesc /></p>
      <ul class="legend unstyled">
        <fields>
          <li><span class="color img-rounded" style="background: ${fieldColor};"></span> <span class="alias"><fieldAlias /></span></li>
        </fields>
      </ul>
    </div>
  </div>
  <img onError="$(this).hide();" alt="" class="graph-image" style="height: ${height}px; width: 100%" data-resize="true" data-src="${graphUrl}">
</div>
