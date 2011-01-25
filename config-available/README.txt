---+ Configuration examples

The   <cliopatria>/config-available   provides   example   configuration
modules. Some of them can simply be  copied to the target config-enabled
directory, while others should be copied and   edited to suit your needs
because there are no sensible  defaults.  If   the  file  is followed by
(edit), copying and subsequent editing is assumed.

The system reads config files from  *|config-enabled|* in the ClioPatria
top-directory and *|config-enabled|* in the local working directory.

Files are loaded in alphabetical order (case sensitive). The config file
prefixes is named =|00-prefixes.pl|= because it often needs to be loaded
first. The ordering of the  other  files   in  this  directory  does not
matter.

  $ 020-prefixes.pl (edit) :
  Configure RDF prefixes (namespaces).  This should typically be loaded
  early in the process.

  $ cache.pl (edit) :
  Control caching of RDF inputs.

  $ config.pl :
  Web-based configuration management.

  $ debug.pl :
  Configure the system for easy development.

  $ https.pl (edit) :
  Add access through HTTPS (HTTP over SSL).

  $ localhost.pl :
  Make ClioPatria work on localhost only.  For development on machines
  that are not connected to the internet, sit behind firewalls, etc.

  $ lod.pl (edit) :
  Configure Linked Open Data access.

  $ logging.pl :
  Configure logging HTTP requests.

  $ network.pl (edit) :
  Configure the (HTTP) network settings, notably the *port*.

  $ ontopath.pl (edit) :
  Configure places for finding RDF libraries.

  $ winpath.pl :
  Configure the locations for finding dot.exe and git.exe on MS-Windows.

  $ zlib.pl :
  Enable loading of compressed (=|.gz|=) RDF data without decompressing.

---++ Plugins

  $ EDM.pl :
  Load examples/customise/EDM.pl, providing additional support to
  vizualise the Europeana Data Model.  See http://www.europeana.eu/portal/


---++ Structure of a config file

Each config file is  a  module   named  conf_<filebase>.  Typically, the
module has a PlDoc comment that  indicates   the  purpose of the module.
Otherwise, they are ordinary Prolog  modules   that  have  to follow all
rules that apply to them. Modules that   define  hooks must include this
text:

    ==
    :- use_module(cliopatria(hooks)).
    ==
