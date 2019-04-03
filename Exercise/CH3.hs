-- following the examples in chapter 3
type CustomerID = Int
type Address = [String]

data Customer = Customer {
    customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
    }deriving (Show)

customer1 = Customer 271828 "J.R.Hacker" ["11", "Haines", "Street"]
customer2 = Customer {
    customerID = 278888,
    customerAddress = ["11", "Haines", "Street"],
    customerName = "RS"
}
