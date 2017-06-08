# RubyPressure

This is my first try as a Shiny app. It computes:
- The pressure if you give it values of lambda, omega0=omega(P=0), and omega_ruby
- The expected Raman shift for the ruby, omega_ruby, if you give it the other info
- The pressure if you upload a ruby spectrum

Either go on the Shiny server: https://cbousige.shinyapps.io/rubypressure/

Or run it directly in R with:
```
require(shiny)
runGitHub("RubyPressure","colinbousige")
```

Or clone the repo, `setwd()` to the folder where all the files are, and run it in R with:
```
require(shiny)
runApp()
```
I even went so far as providing a sample ruby spectrum: try it out!  
The laser wavelength for this experiment was 568.2 nm.