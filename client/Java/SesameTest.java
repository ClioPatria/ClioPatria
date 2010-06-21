// org.openrdf.sesame.Sesame

import java.net.URL;

import org.openrdf.sesame.admin.*;
import org.openrdf.sesame.constants.RDFFormat;
import org.openrdf.sesame.query.*;
import org.openrdf.sesame.query.serql.model.SfwQuery;
import org.openrdf.sesame.sail.*;
import org.openrdf.sesame.repository.remote.*;
import org.openrdf.sesame.constants.QueryLanguage;
import org.openrdf.model.Value;
import org.openrdf.util.io.IOUtil;

class SesameTest
{ public static void main(String argv[])
  { try
    { System.setProperty("org.xml.sax.driver",
			 "org.apache.crimson.parser.XMLReaderImpl");
      
      HTTPService service = new HTTPService(new URL("http://localhost:3020/"));
      HTTPRepository repository = (HTTPRepository)service.getRepository("default");
      service.login("guest", "gasten");
  
      String query = "select * from {x} p {y}";
      QueryResultsTable resultsTable = repository.performTableQuery(QueryLanguage.SERQL, query);
  
      for (int row = 0; row < resultsTable.getRowCount(); row++)
      { for (int column = 0; column < resultsTable.getColumnCount(); column++)
	{ Value value = resultsTable.getValue(row, column);
  
	  System.out.print(value.toString());
	  System.out.print(" ");
	}
	System.out.println();
      }

      service.logout();
    } catch (Exception e)
    { e.printStackTrace();
    }
  }
}
