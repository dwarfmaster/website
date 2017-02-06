
module Config
    @user   = "ruby"
    @pass   = "ruby"
    @dbname = "blog"
    @rcsdir = "/home/luc/Log/programmation/ruby/blog/rcs"

    def self.user
        return @user
    end

    def self.pass
        return @pass
    end

    def self.dbname
        return @dbname
    end

    def self.rcs
        return @rcsdir
    end
end

