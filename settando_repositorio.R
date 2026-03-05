# Pacotes ----

library(usethis)

# Iniciando o Git ----

usethis::use_git()

# Configurando usuário e e-mail----

usethis::use_git_config(user.name = "Edbbioeco",
                        user.email = "edsonbbiologia@gmail.com")

# Settando projeto ----

usethis::proj_get()

# Settando o repositório ----

usethis::use_git_remote(name = "anuros_caatinga",
                        url = "https://github.com/Edbbioeco/anuros_caatinga.git",
                        overwrite = TRUE)


