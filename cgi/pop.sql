USE blog;

CREATE TABLE IF NOT EXISTS Articles(
    id INT PRIMARY KEY AUTO_INCREMENT,
    content TEXT,
    time DATETIME
    );

CREATE TABLE IF NOT EXISTS Tags(
    article INT,
    tag VARCHAR(30)
    );

