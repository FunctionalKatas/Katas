using System.Collections.Generic;

namespace CsRopExample.SqlDatabase
{
    /// <summary>
    /// This class represents a (in-memory) SQL database
    /// </summary>
    class DbContext
    {
        // in-memory collection
        static readonly Dictionary<int, DbCustomer> Data = new Dictionary<int, DbCustomer>();

        
        public IEnumerable<DbCustomer> Customers()
        {
            return Data.Values;
        }

        public void Update(DbCustomer customer)
        {
            if (!Data.ContainsKey(customer.Id))
            {
                // Emulate a SQL error
                throw new SqlException("KNF"); //KeyNotFound
            }

            // use the customer id to trigger some special cases
            switch (customer.Id)
            {
                case 42:
                    // Emulate a SQL error
                    throw new SqlException("TO"); // Timeout
                case 43:
                    // Emulate a SQL error
                    throw new SqlException("AE");  // AuthenticationError
                default:
                    Data[customer.Id] = customer;
                    break;
            }
        }

        public void Insert(DbCustomer customer)
        {
            if (Data.ContainsKey(customer.Id))
            {
                // Emulate a SQL error
                throw new SqlException("DK");  // DuplicateKey
            }

            // use the customer id to trigger some special cases
            switch (customer.Id)
            {
                case 42:
                    // Emulate a SQL error
                    throw new SqlException("TO");  // Timeout
                case 43:
                    // Emulate a SQL error
                    throw new SqlException("AE");  // AuthenticationError
                default:
                    Data[customer.Id] = customer;
                    break;
            }

        }

    }
}
