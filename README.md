# Vega-lite [![build](https://github.com/AestheticIntegration/vega-lite/actions/workflows/main.yml/badge.svg)](https://github.com/AestheticIntegration/vega-lite/actions/workflows/main.yml)

Library to produce [vega-lite](https://vega.github.io/vega-lite/) visualizations.

- vega lite docs: https://vega.github.io/vega-lite/docs/

<script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-lite@4"></script>
<!-- Import vega-embed -->
<script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>

<div style="display: none;">
  <div name="t1"></div>
  <![CDATA[
  <script>
    var spec = "https://raw.githubusercontent.com/aestheticintegration/vega-lite/master/tests/t1.expected.json";
    vegaEmbed('#t1', spec).then(function(result) {
      // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view
    }).catch(console.error);

  </script>
  ]]>
</div>
