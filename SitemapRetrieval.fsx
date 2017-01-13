#load """paket-files/include-scripts/net452/include.main.group.fsx"""
#r "System.Xml.Linq.dll"

[<AutoOpen>]
module Utils =
  open System.IO
  let createIfNotExists dir =
        if not <| Directory.Exists dir then Directory.CreateDirectory ( dir ) |> ignore

[<AutoOpen>]
module SitemapRetrieval =
  open System
  open Hopac
  open HttpFs.Client
  open FSharp.Data
  open Utils

  type Sitemap = XmlProvider<"""<sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"><sitemap><loc>http://www.mouseprice.com/SiteMapHandler/SiteMap1.xml.gz</loc></sitemap></sitemapindex>""">
    
  let getSiteMap () =

    let mousePrice = new Uri "http://www.mouseprice.com/"
    
    let robotsTxt = "robots.txt"

    let robotLink = new Uri ( mousePrice, robotsTxt )

    let getContent uri =
      Request.createUrl Get uri
      |> Request.responseAsString
      |> run

    let getRobotTxt robotUri =
      getContent robotUri

    let parseTxt ( content:string ) =
      let noComments ( line:string ) = line.[0] <> '#'
      let removeEmptyLines ( line:string ) = line.Trim () <> ""
      let toPairs ( line:string ) = 
        let k::v::_ = ( line.Trim () ).Split( [|": "|], StringSplitOptions.None ) |> Seq.toList
        k.Trim (),v.Trim ()
      content.Split ( '\n' )
      |> Seq.filter noComments 
      |> Seq.filter removeEmptyLines
      |> Seq.map toPairs
    
    let getSitemapContent siteMapUri =
      getContent siteMapUri

    let getSitemapFromXml xmlContent =
      let sitemap = Sitemap.Parse xmlContent
      sitemap
    
    let extractSitemapLink =
      getRobotTxt
      >> parseTxt 
      >> Seq.where (fun i -> fst i = "Sitemap")
      >> Seq.exactlyOne 
      >> snd 

    let getSitemapXml = 
      getSitemapContent 
      >> getSitemapFromXml
      
    let result =
      robotLink.ToString () 
      |> extractSitemapLink
      |> getSitemapXml
      
    let locs = 
      result.XElement.Descendants ()
      |> Seq.collect (fun x -> 
            x.Elements () 
            |> Seq.filter (fun ele -> ele.Name.LocalName = "loc")
            |> Seq.map (fun ele -> ele.Value) )
    locs

[<AutoOpen>]
module Downloader =
  open System
  open System.IO
  open System.Net

  let executingDir () = Directory.GetCurrentDirectory ()
  let download link =
    let downloadDir = Path.Combine ( executingDir (), "downloads" )
    createIfNotExists downloadDir
    let uri = 
      (Uri link)
    let fileName () = 
      match uri.Segments |> Seq.toList with
      | "/"::_::[name] -> Some name
      | _ -> None
    match fileName () with
    | Some loc ->
        use cli = new WebClient ()
        cli.AsyncDownloadFile ( uri, Path.Combine ( executingDir (), "downloads", loc ) ) |> Async.RunSynchronously
    | None -> ()

  let downloadGzips all =
    all
    |> Seq.iter download

[<AutoOpen>]
module Unzipper =
  open System.IO
  open System.IO.Compression
  open Utils

  let unzip gzip =
    createIfNotExists "unzipped"
    let unzippedFile = Path.Combine ( "unzipped", Path.GetFileNameWithoutExtension ( gzip ) )
    let content =
      seq { let reader = 
              new StreamReader 
                ( new GZipStream
                    ( File.OpenRead ( gzip ), 
                      CompressionMode.Decompress ) )
            while not reader.EndOfStream do
              yield reader.ReadLine ()
            reader.Close ()
            reader.Dispose () }
    File.AppendAllLines ( unzippedFile , content )

[<AutoOpen>]
module MousePricePropertyGetter =
  open SitemapRetrieval
  open Downloader
  open Unzipper

  let getProperties () = getSiteMap () |> downloadGzips




  // unzip "/Users/markgray/dev/MousePriceScraper/downloads/SiteMap1.xml.gz"