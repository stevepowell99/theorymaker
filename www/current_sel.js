      $(document).on("keydown", function(e) {
        minChars = 1;
        tag1 = document.activeElement.getAttribute("id");
        val1 = document.activeElement.value;
        if (tag1 == "factor1_main-selectized") {
          if (Math.sign(val1.length +1 - minChars) == 1) {
            obj = { "val": val1 };
            Shiny.onInputChange("typed_text", obj);
          }
        }
      });
