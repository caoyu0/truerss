require 'rake'

task :compile do
  c_to = "#{Dir.pwd}/src/main/resources/javascript/"
  path = "#{Dir.pwd}/src/main/resources/javascript/app"
  files = ["ext", "feeds_controller", "ws_controller",
    "contoller_ext", "main_controller", "models", "sources_controller",
    "system_controller",
    "templates", "app"
   ].map{|f| "#{path}/#{f}.coffee"}.join(" ")
  system("cat #{files} | coffee -c -b --stdio > #{c_to}/truerss.js")
end

task :default => :compile
