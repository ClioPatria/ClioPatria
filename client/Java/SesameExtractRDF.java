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
import org.openrdf.util.io.IOUtil;
import org.openrdf.sesame.constants.RDFFormat;
import java.io.InputStream;
import java.io.File;

class SesameExtractRDF
{ public static void main(String argv[])
  { try
    { System.setProperty("org.xml.sax.driver",
			 "org.apache.crimson.parser.XMLReaderImpl");
      
      HTTPService service = new HTTPService(new URL("http://localhost:3020/"));
      HTTPRepository repository = (HTTPRepository)service.getRepository("default");
      StdOutAdminListener listener = new StdOutAdminListener();
      service.login("guest", "gasten");

      try
      { InputStream stream = repository.extractRDF(RDFFormat.RDFXML, true, true, true, true);
	// get export filename
	File modelFile = new File("triples.rdf");
	IOUtil.writeToFile(stream, modelFile);
      } catch (Exception e)
      { e.printStackTrace();
      }
  
      service.logout();
    } catch (Exception e)
    { e.printStackTrace();
    }
  }
}
