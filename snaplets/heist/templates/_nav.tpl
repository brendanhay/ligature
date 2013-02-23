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
          <fromLink span="-1month" text="Month" />
          <fromLink span="-1week" text="Week" />
          <fromLink span="-1day" text="Day" />
          <fromLink span="-1hour" text="Hour" />
        </ifDashboard>
      </ul>

      <ifDashboard>
        <div class="pull-right">
          <span class="time"></span>
        </div>
      </ifDashboard>

    </div>
  </div>
</div>
