
module Config
    @user   = "ruby"
    @pass   = "ruby"
    @dbname = "blog"
    @artdir = "/home/luc/Log/programmation/ruby/blog/articles"

    def self.user
        return @user
    end

    def self.pass
        return @pass
    end

    def self.dbname
        return @dbname
    end

    def self.artdir
        return @artdir
    end
end

