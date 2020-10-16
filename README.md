# Track Investments over Time with a Shiny App

This Shiny app is designed to help users track investments over time and explore funds in multiple locations together.

The app takes in a file called "Retirement" which has the following columns:
  - Investments: The investment name
  - Rate: Morningstar rating of the fund
  - Fee: Fee paid to maintain the fund
  - Location: Bank of firm holding the fund
  - Description: What type of fund (stock, bond, cash, etc)
  - Shares: How many individual shares owned
  - Date columns, by month: Displays the value of the investment for that month

It returns:
  - Pie chart of investment type by value for past month
  - Pie chart of investment area by value for past month
  - Line graph of investment value over time for all individual investments
  - Line graph of investment value over time for investment types
  - Bar chart of morningstar ratings
  - Scatterplot of investment fees
  - Datatable displaying information
  
  Additionally, almost all columns can be filtered on, letting you look closely at sections of investments at a time!
  
 ![dashboard_screenshot](https://github.com/mathyjokes/Finance_Dashboard/blob/main/dashboard_screenshot_edited.jpg)
