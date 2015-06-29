USE blog;

CREATE TABLE IF NOT EXISTS Articles(
    id INT PRIMARY KEY AUTO_INCREMENT,
    path VARCHAR(25),
    time DATETIME
    );
INSERT INTO Articles(path, time) VALUES
    ("introduction", "2014-09-19 12:00:00"),
    ("lorem",        "2014-09-19 15:59:59"),
    ("archlinux",    "2015-06-29 10:12:05");

CREATE TABLE IF NOT EXISTS Tags(
    article INT,
    tag VARCHAR(5)
    );
INSERT INTO Tags(article, tag) VALUES
    (1, "test"),
    (1, "prog"),
    (2, "test"),
    (3, "linux"),
    (3, "prog");

