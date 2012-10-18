disable "Remote Desktop Connection"
disable /VirtualBox/

start_at_login(true)

def reload_configuration
  lambda{ alert("Configuration reloaded successfully") if reload}
end

only /Chrome/, /X11/ do
  map ",," do
    app_title = Accessibility::Gateway.get_active_application.title
    if app_title =~/Chrome/
      activate("X11");
    else
      activate("Google Chrome")
      sleep(0.25)
      send("<Cmd-r>")
    end
  end
  map ",m" do
    app_title = Accessibility::Gateway.get_active_application.title
    if app_title =~/Chrome/
      activate("X11");
    else
      activate("Google Chrome")
    end
  end
end

Thread.new do
  sleep(1800)
 `nohup killall Keymando && open /Applications/Keymando.app`
end.run

map "<Cmd-.>" do
  input({
    "reload" => reload_configuration,
    "restart" => lambda {
      `nohup killall Keymando && open /Applications/Keymando.app`
    },
    "gc" => lambda {
      GC.enable
      GC.start
      GC.disable
    },
    "foo" => lambda { alert("hello world") }
  })
end
