
# Function to automate modifications to HTML files
automate_html_modifications <- function(input_file_path, output_file_path) {
  # Read the HTML content
  html_content <- readLines(input_file_path, warn = FALSE)
  html_content <- paste(html_content, collapse = "\n")
  
  # Perform replacements
  html_content <- gsub("è", "\\\\u00e8", html_content, fixed = TRUE)
  
  # CSS replacements
  html_content <- gsub(".d3-slider-axis text {\\s+font-size: 11px;\\s+}", 
                       ".d3-slider-axis text {\n    font-size: 0px;\n}", 
                       html_content, perl = TRUE)
  
  html_content <- gsub(".main, .xlab {\\s+text-anchor: middle;\\s+}", 
                       ".main {\n  text-anchor: middle;\n  font-size: 1.6em;\n}\n\n.xlab {\n  text-anchor: middle;\n  font-size: 1.6em;\n  font-weight: bold\n}", 
                       html_content, perl = TRUE)
  
  html_content <- gsub(".label, .main, .xlab, .tooltip {\\s+font-family: sans-serif;\\s+}", 
                       ".label, .main, .xlab {\n  font-family: 'calibri', 'sans-serif';\n}\n\n.tooltip {\n  font-family: 'calibri', 'sans-serif';\n}", 
                       html_content, perl = TRUE)
  
  # JavaScript replacements for slider and interval adjustments
  html_content <- gsub("var durationSlider = d3.slider\\(\\).min\\(0\\).max\\(8\\).axis\\(new d3.svg.axis\\(\\).ticks\\(5\\)\\).value\\(n3.options.animationDuration/1000\\);", 
                       "var durationSlider = d3.slider().min(0).max(2).axis(new d3.svg.axis().ticks(5)).value(n3.options.animationDuration/1000);", 
                       html_content)
  
  html_content <- gsub("interval=1;", "interval=0.1;", html_content)
  
  # More detailed replacements for animation duration control
  html_content <- gsub("durationControl.append\\('span'\\).attr\\('class', 'menu-label'\\).html\\('Animation Duration'\\);", 
                       "durationControl.append('span').attr('class', 'menu-label').html('Slow down animation &#8594');", 
                       html_content)
  
  html_content <- gsub("var durationSlider = d3.slider\\(\\).min\\(0\\).max\\(8\\).axis\\(new d3.svg.axis\\(\\).ticks\\(5\\)\\).value\\(n3.options.animationDuration/1000\\)", 
                       "var durationSlider = d3.slider().min(0.5).max(5).axis(new d3.svg.axis().ticks(5)).value(n3.options.animationDuration/800)", 
                       html_content)
  
  html_content <- gsub("n3.options.animationDuration = value\\*1000", 
                       "n3.options.animationDuration = value*800", 
                       html_content)
  
  html_content <- gsub("interval=1;", "interval=0.5;", html_content)
  
  # Ticks replacement
  html_content <- gsub(".ticks\\(Math.round\\(sliderLength / 100\\)\\)", 
                       ".ticks(Math.round(sliderLength / 50))", 
                       html_content)
  
  html_content <- gsub("n3.domTarget.select\\('.xlab'\\).attr\\('transform', \"translate\\(\"\\+center\\+\",\"\\+\\(div_height-margin.y\\)\\+\"\\)\"\\)", 
                       "n3.domTarget.select('.xlab').attr('transform', \"translate(\"+center+\",\"+(div_height-margin.y*2)+\")\")", 
                       html_content)
  
  # Write the modified HTML content back to a new file
  writeLines(html_content, output_file_path)
}

# Example usage
input_file_path <- paste0(output_filename, ".html")
output_file_path <- paste0(output_filename, "_mod.html")
automate_html_modifications(input_file_path, output_file_path)