library(shiny)
library(plotly)

shinyUI(
  fluidPage(
    titlePanel('Bayesian A/B/n test for ARPU'),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          'rev_A',
          'Total Revenue for Control:',
          min = 0,
          max = 1e6,
          value = 1256.71
        ),
        numericInput(
          'success_A',
          'Control Conversions:',
          min = 0,
          max = 1e6,
          value = 250
        ),
        numericInput(
          'total_A',
          'Total Control Players:',
          min = 0,
          max = 1e6,
          value = 6442
        ),
        numericInput(
          'retention_A',
          'Retention A:',
          min = 0,
          max = 1e6,
          value = 2101
        ),
        numericInput(
          'rev_B',
          'Total Revenue Treatment 1:',
          min = 0,
          max = 1e6,
          value = 1157.59
        ),
        numericInput(
          'success_B',
          'Converted Players Treatment 1:',
          min = 0,
          max = 1e6,
          value = 274
        ),
        numericInput(
          'total_B',
          'Total Players Treatment 1:',
          min = 0,
          max = 1e6,
          value = 6442
        ),
        numericInput(
          'retention_B',
          'Retention B:',
          min = 0,
          max = 1e6,
          value = 2166
        ),
        numericInput(
          'rev_C',
          'Total Revenue Condition 2:',
          min = 0,
          max = 1e6,
          value = 1316.22
        ),
        numericInput(
          'success_C',
          'Converted Players Condition 2:',
          min = 0,
          max = 1e6,
          value = 335
        ),
        numericInput(
          'total_C',
          'Total Players in Condition 2:',
          min = 0,
          max = 1e6,
          value = 6423
        ),
        numericInput(
          'retention_C',
          'Retention C:',
          min = 0,
          max = 1e6,
          value = 2164
        ),
        numericInput(
          'rev_D',
          'Total Revenue Condition 3:',
          min = 0,
          max = 1e6,
          value = 1828.35
        ),
        numericInput(
          'success_D',
          'Converted Players Condition 3:',
          min = 0,
          max = 1e6,
          value = 288
        ),
        numericInput(
          'total_D',
          'Total Players in Condition 3:',
          min = 0,
          max = 1e6,
          value = 6335
        ),
        numericInput(
          'retention_D',
          'Retention D:',
          min = 0,
          max = 1e6,
          value = 2047
        ),
        numericInput(
          'rev_E',
          'Total Revenue Treatment 4:',
          min = 0,
          max = 1e6,
          value = 1098.13
        ),
        numericInput(
          'success_E',
          'Converted Players Treatment 4:',
          min = 0,
          max = 1e6,
          value = 312
        ),
        numericInput(
          'total_E',
          'Total Players Treatment 4:',
          min = 0,
          max = 1e6,
          value = 6555
        ),
        numericInput(
          'retention_E',
          'Retention E:',
          min = 0,
          max = 1e6,
          value = 2153
        ),
        numericInput(
          'sim_sample',
          'Monte-Carlo MC Samples:',
          min = 1,
          max = 1e7,
          value = 1e5
        ),
        actionButton(
          'button',
          'Calculate'
        ),
        hr(),
        tags$div(
          class='header', checked = NA,
          tags$p(
            'Bayesian test for A/B/n ARPU calculator. Modified for internal use'
          ),
          tags$br(),
          tags$a(
            href = 'https://github.com/Vidogreg/bayes-ab-testing/tree/master/bayes-arpu-test',
            'Original Code by V. Gregor'
          ),
          tags$p('')
        )
      ),
      mainPanel(
        tableOutput('table1'),
        tableOutput('table2'),
        tableOutput('table3'),
        tableOutput('table4')
      )
    )
  )
)
