#Pry.config.should_load_plugins = false

def load_rails
  require 'benchmark'
  puts "Starting Rails Environment..."
  time = Benchmark.measure do
    rails = File.join Dir.getwd, 'config', 'environment.rb'

    if File.exist?(rails) && ENV['SKIP_RAILS'].nil?
      require rails

      if Rails.version[0..0] == "2"
        require 'console_app'
        require 'console_with_helpers'
      elsif Rails.version[0..0] == "3"
        require 'rails/console/app'
        require 'rails/console/helpers'

        # For Rails 3.2
        if defined?(Rails) && Rails.env
          extend Rails::ConsoleMethods
        end
      else
        warn "[WARN] cannot load Rails console commands (Not on Rails2 or Rails3?)"
      end
    end
  end
  puts "Time taken #{time}"
end
alias :rl :load_rails
