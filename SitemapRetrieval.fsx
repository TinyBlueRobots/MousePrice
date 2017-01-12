#load """paket-files/include-scripts/net452/include.main.group.fsx"""
#r "System.Xml.Linq.dll"
[<AutoOpen>]
module SitemapRetrieval =
  open System
  open Hopac
  open HttpFs.Client
  open FSharp.Data
  open Ionic

  type Sitemap = XmlProvider<"""<sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"><sitemap><loc>http://www.mouseprice.com/SiteMapHandler/SiteMap1.xml.gz</loc></sitemap></sitemapindex>""">
    
  let getSiteMap () =

    let mousePrice = new Uri "http://www.mouseprice.com/"
    
    let robotsTxt = "robots.txt"

    let robotLink = new Uri ( mousePrice, robotsTxt)

    let getContent uri =
      Request.createUrl Get uri
      |> Request.responseAsString
      |> run

    let getRobotTxt robotUri =
      getContent robotUri

    let parseTxt (content:string) =
      let noComments (line:string) = line.[0] <> '#'
      let removeEmptyLines (line:string) = line.Trim() <> ""
      let toPairs (line:string) = 
        let k::v::_ = (line.Trim()).Split([|": "|], StringSplitOptions.None) |> Seq.toList
        k.Trim(),v.Trim()
      content.Split('\n')
      |> Seq.filter noComments 
      |> Seq.filter removeEmptyLines
      |> Seq.map toPairs
    
    let getSitemapContent siteMapUri =
      getContent siteMapUri

    let getSitemapFromXml xmlContent =
      let sitemap = Sitemap.Parse(xmlContent)
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
      robotLink.ToString() 
      |> extractSitemapLink
      |> getSitemapXml
      
    let locs = 
      result.XElement.Descendants()
      |> Seq.collect (fun x -> 
            x.Elements() 
            |> Seq.filter (fun ele -> ele.Name.LocalName = "loc")
            |> Seq.map (fun ele -> ele.Value) )
    locs
