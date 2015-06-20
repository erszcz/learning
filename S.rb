#!/usr/bin/env ruby
# encoding: utf-8

class S
    #class << self
    #    private :new
    #end

    private :new
    #private :initialize

    def self.instance
        @instance ||= new
    end
end

if __FILE__ == $0
    #s = S.instance
    s = S.instance
    s2 = S.instance
    p s.inspect, s2.inspect
end
