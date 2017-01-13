#load """paket-files/include-scripts/net452/include.main.group.fsx"""
#r "System.Xml.Linq.dll"

[<AutoOpen>]
module Utils =
  open System.IO
  let createIfNotExists dir =
        if not <| Directory.Exists dir then Directory.CreateDirectory ( dir ) |> ignore

  let getUnzippedFiles () = 
    Directory.GetFiles ( "unzipped" )

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
        let downloadedGzip = Path.Combine ( downloadDir, loc )
        async 
          { do! cli.AsyncDownloadFile ( uri, downloadedGzip )
            return downloadedGzip }
        |> Some
    | None -> None

  let downloadGzips all =
    all
    |> Seq.map download
    |> Seq.filter Option.isSome
    |> Seq.map Option.get

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
    unzippedFile

[<AutoOpen>]
module Reader =
  open System.IO
  open FSharp.Data
  open Utils

  type Properties = XmlProvider<"""<?xml version="1.0" encoding="utf-8"?><urlset xmlns="http://www.google.com/schemas/sitemap/0.9"><url><loc>http://www.mouseprice.com/area-guide/AB10</loc></url></urlset>""">

  let checkForSales ( text : string ) =
    text.Contains "for-sale"

  let textToXml ( reader : TextReader ) =
    let x = Properties.Load reader
    x.XElement.Descendants ()
    |> Seq.filter ( fun e -> e.Name.LocalName = "url" )
    |> Seq.map ( fun e -> e.Value )
  
  let checkXml ( filename : string ) =
    let file = sprintf "/Users/markgray/dev/MousePriceScraper/%s" filename
    let rdr = new StreamReader ( File.OpenRead file ) // :> TextReader
    let read = textToXml rdr
    rdr.Close ()
    rdr.Dispose ()
    read |> Seq.exists checkForSales

  let loadAndCheckFiles files =
    files
    |> Seq.map checkXml
    |> Seq.reduce (||)

(*
   Reader.checkXml "/Users/markgray/dev/MousePriceScraper/unzipped/SiteMap1.xml"
   textToXml it
*)

[<AutoOpen>]
module MousePricePropertyGetter =
  open SitemapRetrieval
  open Downloader
  open Unzipper
  open Reader

  let asyncArray (sitemaps:seq<Async<string>>) = 
    let sms=
      [| for sitemap in sitemaps do
          yield
              async {
                return! sitemap }
              |> Async.RunSynchronously |]
    downloadGzips sms
  
  let unzipArray = Array.map unzip
  let mapper (item:seq<Async<string>>) =
    let unzipper a = async { return unzipArray a } 
    let asyncs =
      item
      |> asyncArray
      |> Seq.map Async.RunSynchronously
      |> Seq.toArray
      |> unzipper

    Async.Parallel item

  let getProperties size = 
    let xxx =
      async {
        let! props = 
          getSiteMap () 
          |> Seq.take size
          |> downloadGzips
          |> Async.Parallel
        return props |> unzipArray
      }
    xxx
    |> Async.RunSynchronously
    |> loadAndCheckFiles

  // Delete "downloads" & "unzipped" then run the next line
  // getProperties 550

  // unzip "/Users/markgray/dev/MousePriceScraper/downloads/SiteMap6.xml.gz"
