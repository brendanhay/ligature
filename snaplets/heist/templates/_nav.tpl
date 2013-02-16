<div class="navbar navbar-inverse navbar-fixed-top">
  <div class="navbar-inner">
    <div class="container">

      <a class="brand" href="/"><pageTitle /></a>

      <ul class="nav">
        <li class="dropdown">
          <a href="#" class="dropdown-toggle" data-toggle="dropdown">Dashboard <b class="caret"></b></a>
          <ul class="dropdown-menu">
            <nav>
              <li><a href="${navLink}" alt="${navDesc}"><navName /></a></li>
            </nav>
          </ul>
        </li>

        <ifDashboard>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">Time <b class="caret"></b></a>
            <ul class="dropdown-menu">
              <li><a href="${currentUrl}?from=month">Month</a></li>
              <li><a href="${currentUrl}?from=week">Week</a></li>
              <li><a href="${currentUrl}?from=day">Day</a></li>
              <li><a href="${currentUrl}?from=hour">Hour</a></li>
            </ul>
          </li>
        </ifDashboard>
      </ul>

    </div>
  </div>
</div>
