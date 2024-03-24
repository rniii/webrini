require "rake/clean"

CLOBBER.include("pages/*.html")
CLOBBER.include("pages/*.gz")
CLOBBER.include("static/*.gz")

pages = FileList["pages/*.haml"]

task :build => [:build_client, :compress]

task :build_client => pages.ext(".html") do |task|
  sh "dune build"
  sh "esbuild --bundle _build/default/output/client/client.js | \
    uglifyjs -c unsafe,unsafe_comps,passes=8 -m -o static/index.js"
  sh "esbuild --minify css/index.css --outdir=static"
end

task :compress => [*pages.ext(".html.gz"), "static/index.js.gz", "static/index.css.gz"]

rule ".html" => [".haml"] do |task|
  sh "haml render #{task.source} > #{task.name}"
end

rule ".gz" => [proc {|t| t.delete_suffix ".gz"}] do |task|
  sh "zopfli #{task.source}"
end
