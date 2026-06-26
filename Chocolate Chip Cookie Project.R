library(tidyverse)
library(caret)
library(randomForest)
library(rvest)
library(httr2)

recipes <- read_csv("RAW_recipes.csv")
interactions <- read_csv("RAW_interactions.csv")

cookies <- recipes %>%
  filter(str_detect(str_to_lower(name), "chocolate (chip|chips|chunk|chunks)")) %>%
  filter(str_detect(str_to_lower(name), "cookie")) %>%
  filter(!str_detect(str_to_lower(name), "bar|bars|brownie|brownies|cake|cupcake|cupcakes|cheesecake|pie|pizza|muffin|muffins|milkshake|ice cream|truffle|truffles|dip|fudge|brittle|sandwich|sandwiches|pots|dessert|whip|dough|drinkable|raw|jar|baked oatmeal|cookie oatmeal|jumbo|mix|log|mug|balls|pan|skillet"))

cookies <- cookies %>%
  mutate(urlName = name %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_replace_all("\\s+", "-"))

cookies <- cookies %>%
  mutate(url = paste0("https://www.food.com/recipe/", urlName, "-", id))

scrape_recipe <- function(url, recipe_id) 
{
  tryCatch(
  {
    page <- read_html(url)
    quantities <- page %>%
      html_elements(".ingredient-quantity") %>%
      html_text(trim = TRUE)
    ingredients <- page %>%
      html_elements(".ingredient-text") %>%
      html_text(trim = TRUE)
    tibble(recipe_id = recipe_id, quantity = quantities, ingredient = ingredients)
  }, error = function(e) 
    {
    cat("FAILED:", recipe_id, "\n")
    tibble(recipe_id = recipe_id, quantity = NA_character_, ingredient = NA_character_, failed = TRUE)
    })
}

results <- list()

for(i in seq_len(nrow(cookies))) 
{
  results[[i]] <- scrape_recipe(cookies$url[i], cookies$id[i])
  Sys.sleep(.25)
  if(i %% 25 == 0) 
  {
    temp <- bind_rows(results)
    write_csv(temp, "cookie_scrape_progress.csv")
  }
}

ingredient_data <- bind_rows(results)

failed_ids <- c(cookies$id[277], cookies$id[454])

failed_recipes <- ingredient_data %>%
  filter(is.na(quantity)) %>%
  distinct(recipe_id)

ingredient_data <- ingredient_data %>%
  filter(!recipe_id %in% failed_ids)

write_csv(ingredient_data, "cookie_ingredients_raw.csv")

ratings <- interactions %>%
  group_by(recipe_id) %>%
  summarize(avg_rating = mean(rating), n_ratings = n())

C <- weighted.mean(ratings$avg_rating, ratings$n_ratings)
m <- 10

ratings <- ratings %>%
  mutate(bayesian_score = (n_ratings / (n_ratings + m)) * avg_rating +(m / (n_ratings + m)) * C)

junk_ingredients <- c("parchment paper", "drops green food coloring", "food-grade diatomaceous earth", "cooking spray", "pam cooking spray", "nonstick cooking spray")

ingredient_data <- ingredient_data %>%
  filter(!ingredient_final %in% junk_ingredients)

ingredient_data <- ingredient_data %>%
  mutate(
    unit = case_when(
      str_detect(ingredient, "^cups?") ~ "cup",
      str_detect(ingredient, "^tablespoons?") ~ "tbsp",
      str_detect(ingredient, "^teaspoons?") ~ "tsp",
      str_detect(ingredient, "^ounces?") ~ "oz",
      str_detect(ingredient, "^g\\b") ~ "g",
      str_detect(ingredient, "^lb\\b") ~ "lb",
      str_detect(ingredient, "^lbs\\b") ~ "lbs",
      TRUE ~ "item")) %>%
  mutate(
    ingredient_raw = ingredient %>%
      str_remove("^cups?\\s+") %>%
      str_remove("^tablespoons?\\s+") %>%
      str_remove("^teaspoons?\\s+") %>%
      str_remove("^ounces?\\s+") %>%
      str_remove("^g\\s+") %>%
      str_remove("^lb\\s+") %>%
      str_remove("^large\\s+") %>%
      str_remove("^lbs\\s+")) %>%
  mutate(
    ingredient_std =
      ingredient_raw %>%
      str_to_lower() %>%
      str_remove_all("\\(.*?\\)") %>%
      str_remove(",.*") %>%
      str_squish()) %>%
  mutate(ingredient_std = str_remove(ingredient_std, "\\s+or\\s+.*$")) %>%
  mutate(quantity = case_when(
    ingredient_std == "and 3 tablespoons flour" & quantity == "2" ~ "2.1875", ingredient_std == "plus 2 tablespoons sugar" & quantity == "3⁄4" ~ "0.875", ingredient_std == "tbsps all-purpose flour" & quantity == "2" ~ "2.125", ingredient_std == "3/4 cup butter" & quantity == "1" ~ "1.75", TRUE ~ quantity),
    ingredient_final = case_when(
      ingredient_std %in% c("flour", "all-purpose flour", "unbleached all-purpose flour", "unsifted flour", "unbleached flour", "plain flour", "white flour", "unsifted all-purpose flour", "sifted all-purpose flour", "bleached all purpose flour", "sifted flour", "all-purpose white flour", "plain white flour", "unbleached white flour", "gold medal all-purpose flour", "all-purpose gold medal flour", "and 3 tablespoons flour","tbsps all-purpose flour") ~ "all_purpose_flour",
      ingredient_std == "bread flour" ~ "bread_flour",
      ingredient_std %in% c("blended oatmeal", "oat flour") ~ "oat_flour",
      ingredient_std %in% c("all-purpose gluten-free flour", "gluten-free flour", "brown rice flour", "rice flour", "sweet rice flour") ~ "gluten_free_flour",
      ingredient_std %in% c("self-rising flour", "self raising flour", "self-raising flour") ~ "self_rising_flour",
      ingredient_std %in% c("cake flour", "softassilk cake flour") ~ "cake_flour",
      ingredient_std %in% c("blanched almond flour", "almond meal", "almond flour") ~ "almond_flour",
      ingredient_std %in% c("whole wheat flour", "whole what flour", "white whole wheat flour", "whole wheat pastry flour", "hard whole wheat flour", "fresh ground soft whole wheat pastry flour", "wheat flour", "plain whole meal flour") ~ "whole_wheat_flour",
      ingredient_std %in% c("brown sugar", "packed brown sugar", "firmly packed brown sugar", "light brown sugar", "packed light brown sugar", "firmly packed light brown sugar", "soft brown sugar", "lightly packed brown sugar", "soft light brown sugar", "packed light-brown sugar", "packed golden brown sugar", "golden brown sugar", "light-brown sugar", "firmly packed light-brown sugar", "firm packed light brown sugar", "tightly packed light brown sugar", "loosely packed brown sugar", "light muscovado sugar", "firmly-packed brown sugar", "sucanat", "turbinado sugar") ~ "light_brown_sugar",
      ingredient_std %in% c("dark brown sugar", "packed dark brown sugar", "firmly packed dark brown sugar", "lightly packed dark brown sugar", "well-packed dark brown sugar") ~ "dark_brown_sugar",
      ingredient_std %in% c("sugar", "white sugar", "additional sugar", "granulated sugar", "caster sugar", "superfine sugar", "super extra finely granulated sugar", "raw sugar", "plus 2 tablespoons sugar", "unbleached cane sugar", "unrefined sugar", "baker's sugar", "vanilla sugar", "granular fructose") ~ "sugar",
      ingredient_std %in% c("butter", "salted butter", "softened butter", "soft butter", "room temperature butter", "whole butter", "real butter", "sweet butter", "sweet creamy butter", "melted butter", "browned butter", "smart balance butter spread", "smart balance light butter spread", "vegan butter", "3/4 cup butter", "light butter", "two sticks butter", "butter substitute") ~ "salted_butter",
      ingredient_std %in% c("unsalted butter", "cool unsalted butter", "softened unsalted butter", "cold unsalted butter", "sweet unsalted butter", "unsweetened butter") ~ "unsalted_butter",
      ingredient_std %in% c("egg", "eggs", "beaten egg", "whole egg", "extra large egg", "medium eggs", "free-range eggs") ~ "egg",
      ingredient_std %in% c("egg white", "whole egg white", "egg whites", "whote egg white") ~ "egg_white",
      ingredient_std %in% c("egg yolk", "egg yolks") ~ "egg_yolk",
      ingredient_std %in% c("egg beaters egg substitute", "vegan egg substitute", "orgran no egg", "eggs worth bob's red mill egg substitute", "egg substitute") ~ "egg_substitute",
      ingredient_std %in% c("vanilla", "splash vanilla", "vanilla extract", "drops vanilla", "pure vanilla extract", "pure bourbon vanilla", "real vanilla extract", "natural vanilla extract", "vanilla essence", "vanilla flavoring", "ground vanilla", "nielsen-massey madagascar bourbon pure vanilla extract", "kosher for passover vanilla extract", "sugar-free vanilla extract") ~ "vanilla_extract",
      ingredient_std %in% c("bicarbonate of soda", "fresh baking soda", "gluten-free baking soda", "pinch baking soda", "baking soda") ~ "baking_soda",
      ingredient_std %in% c("baking powder", "gluten free baking powder") ~ "baking_powder",
      ingredient_std %in% c("salt", "sea salt", "dash salt", "smallish-medium coarse sea salt", "kosher salt", "pinch salt", "coarse salt", "table salt", "coarse sea salt", "celtic sea salt", "flake salt", "flaked sea salt", "good salt") ~ "salt",
      ingredient_std %in% c("chocolate chips", "chips", "package chocolate chips", "bag chocolate chips", "semi-sweet chocolate chips", "hershy's semi-sweet chocolate chips", "bag nestle semi-sweet chocolate chips", "package semi-sweet chocolate chips", "nestle chocolate chips", "nestles semi-sweet chocolate chips", "nestle toll house semisweet chocolate morsels", "toll house semisweet chocolate morsels", "nestlé® toll house® semi-sweet chocolate morsels", "regular semi-sweet chocolate chips", "semisweet chocolate morsels", "package semisweet chocolate morsels", "cups/ 16 oz semi-sweet chocolate chips", "bag semi-sweet chocolate chips", "bags chocolate chips", "package hershey semi-sweet chocolate chips", "bag ghirardelli semi-sweet chocolate chips", "twelve oz bags semi-sweet chocolate chips", "hershey's semi-sweet chocolate chips", "dagoba chocolate chips", "organic chocolate chips", "plain chocolate chips", "packages chocolate chips", "packet chocolate chips", "ghirardelli chocolate chips", "4 cups semisweet chocolate chips", "semisweet vegan chocolate chips", "non-dairy chocolate chips", "sugar-free chocolate chips", "barley malt sweetened chocolate chips", "chocolate curls", "packages premium chocolate chips", "parve chocolate chips") ~ "semi_sweet_chocolate_chips",
      ingredient_std %in% c("milk chocolate chips", "ghirardelli milk chocolate chips", "package milk chocolate chips", "bag milk chocolate chips", "milk_chocolate") ~ "milk_chocolate_chips",
      ingredient_std %in% c("dark chocolate chips", "ghirardelli bittersweet chocolate chips", "hershey's special dark chocolate chips", "bittersweet chocolate chips", "of ghirardelli's 60% cacao chocolate chips",   "bag dark chocolate chips", "ghiradelli 60% cacao baking chocolate chips", "ghiradelli double chocolate chips") ~ "dark_chocolate_chips",
      ingredient_std %in% c("white chocolate chips", "hershey vanilla chips", "white chips", "bag white chocolate chips", "package white chocolate chips", "nestle semi-sweet and white chocolate swirled chocolate morsels") ~ "white_chocolate_chips",
      ingredient_std %in% c("rolled oats", "oatmeal", "gluten-free oats", "regular rolled oats", "oats", "old fashioned oats", "old-fashioned oatmeal", "rolled oatmeal", "uncooked oats", "uncooked oatmeal", "uncooked rolled oats", "quaker oats", "regular oats", "whole oats", "organic rolled oats", "quaker old fashioned oats", "ground oatmeal", "large-flake oats", "large-flake rolled oats") ~ "old_fashioned_oats",
      ingredient_std %in% c("quick oats", "quick-cooking oats", "quick cooking oatmeal", "uncooked quick-cooking oats", "quick-cooking rolled oats", "quick oatmeal", "quick-cooking oatmeal", "instant oats", "slow cooking oats", "quick-cooking raw oatmeal") ~ "quick_oats",
      ingredient_std %in% c("unsweetened dutch-processed cocoa powder", "cocoa", "cocoa powder", "unsweetened cocoa powder", "unsweetened cocoa", "unsweetened baking cocoa", "natural cocoa", "baker's cocoa", "baking cocoa", "hershey's cocoa powder", "fine cocoa powder", "dutch-processed cocoa powder", "unsweetened dutch cocoa") ~ "cocoa_powder",
      ingredient_std %in% c("walnuts", "chopped walnuts", "2 cups walnuts", "toasted walnuts", "walnut pieces", "chopped raw walnuts","coarsely chopped walnuts", "ground walnuts", "organic walnuts", "chopped toasted walnuts", "finely diced walnuts", "of chopped walnuts") ~ "walnuts",
      ingredient_std %in% c("pecans", "chopped pecans", "1 cup pecans", "toasted pecans", "coarsely chopped pecans", "pecan pieces", "pecan nuts", "coarse chopped pecans", "loosely chopped pecans", "chopped toasted pecans", "of chopped pecans") ~ "pecans",
      ingredient_std %in% c("peanut butter", "creamy peanut butter", "crunchy peanut butter", "chunky peanut butter", "natural peanut butter", "natural-style peanut butter", "skippy creamy peanut butter", "smooth peanut butter", "super chunky peanut butter", "reduced-fat peanut butter", "creamy unsalted peanut butter", "natural creamy peanut butter", "sweet creamy peanut butter", "jar chunky peanut butter", "reduced-calorie peanut butter", "better 'n peanut butter spread") ~ "peanut_butter",
      ingredient_std %in% c("semisweet chocolate chunks", "semisweet chocolate chunk", "package semisweet chocolate chunks", "semisweet chocolate", "semi-sweet chocolate baking squares", "semisweet baking chocolate", "semisweet chocolate piece", "semi-sweet chocolate bit", "semi-sweet chocolate bits", "bag semisweet chocolate chunks", "package nestle semisweet chocolate chunks", "coarsely chopped semisweet chocolate chunks", "package semisweet chocolate", "packages baker semisweet chocolate", "packages semisweet chocolate pieces", "coarsely chopped semisweet chocolate") ~ "semisweet_chocolate_chunks",
      ingredient_std %in% c("white chocolate", "white baking chocolate", "white chocolate baking bar") ~ "white_chocolate_chunks",
      ingredient_std %in% c("chocolate", "milk chocolate", "milk chocolate pieces") ~ "milk_chocolate_chunks",
      ingredient_std %in% c("mini chocolate chip", "mini chocolate chips", "miniature chocolate chip", "miniature chocolate chips", "miniature semisweet chocolate chips", "semisweet mini chocolate chips", "semi-sweet mini morsels chocolate chips", "package semisweet mini chocolate chips", "bag mini chocolate chips", "bag miniature semisweet chocolate chips") ~ "mini_chocolate_chips",
      ingredient_std %in% c("powdered sugar", "confectioners' sugar", "icing sugar") ~ "powdered_sugar",
      ingredient_std %in% c("water", "hot water", "warm water", "boiling water") ~ "water",
      ingredient_std %in% c("applesauce", "unsweetened applesauce", "plain unsweetened applesauce") ~ "applesauce",
      ingredient_std %in% c("oil", "vegetable oil", "light olive oil", "canola oil", "tbsps vegetable oil", "corn oil", "light oil", "salad oil", "sunflower oil", "olive oil", "grapeseed oil") ~ "oil",
      ingredient_std %in% c("banana", "bananas", "mashed banana", "mashed ripe banana", "medium ripe bananas", "medium very ripe banana", "ripe bananas", "ripe mashed banana", "additional banana", "mashed up banana", "small banana") ~ "banana",
      ingredient_std %in% c("milk", "whole milk", "skim milk", "nonfat milk", "1% low-fat milk", "cold milk", "splash milk", "vanilla soymilk", "sour milk") ~ "milk",
      ingredient_std %in% c("chopped nuts", "coarsely chopped nuts", "nuts", "coarsly chopped nuts", "chopper nuts") ~ "mixed_nuts",
      ingredient_std %in% c("coconut", "desiccated coconut", "dried coconut", "shredded unsweetened coconut", "unsweetened coconut", "unsweetened dried shredded coconut", "flaked coconut", "shredded coconut", "sweetened coconut", "sweetened flaked coconut","shredded sweetened coconut") ~ "coconut",
      ingredient_std %in% c("organic virgin coconut oil", "coconut oil") ~ "coconut_oil",
      ingredient_std %in% c("coconut extract", "coconut flavoring") ~ "coconut_extract",
      ingredient_std %in% c("pumpkin", "pumpkin puree", "canned pumpkin", "can pumpkin puree", "can solid pack pumpkin", "can pumpkin", "can libby solid-pack pumpkin") ~ "pumpkin",
      ingredient_std %in% c("stevia", "stevia powder", "white stevia powder", "nustevia white stevia powder") ~ "stevia",
      ingredient_std %in% c("splenda granular", "splenda sugar substitute", "splenda sugar blend for baking", "splenda brown sugar blend", "packed splenda brown sugar blend", "firmly packed splenda brown sugar blend", "sugar substitute") ~ "splenda",
      ingredient_std %in% c("margarine", "1 cup margarine", "softened margarine", "light margarine", "vegan margarine", "corn oil margarine", "canola margarine", "cool margarine", "melted margarine", "reduced-calorie margarine", "soy margarine", "baking margarine", "flieschman's margarine", "non-hydronenized margarine", "light vegan margarine", "stick margarine", "softened butter flavored parve margarine", "promise trans-fat free margarine") ~ "margarine",
      ingredient_std %in% c("ground cinnamon", "freshly grated cinnamon", "pinch cinnamon", "dash cinnamon") ~ "cinnamon",
      ingredient_std %in% c("clove", "ground cloves", "powdered clove") ~ "cloves",
      ingredient_std %in% c("ground ginger", "ginger powder", "powdered ginger", "ginger", "peeled finely grated fresh ginger", "crystallized ginger", "minced crystallized ginger") ~ "ginger",
      ingredient_std %in% c("nutmeg", "ground nutmeg", "grated nutmeg", "freshly grated nutmeg", "pinch nutmeg") ~ "nutmeg",
      ingredient_std %in% c("allspice", "pinch allspice") ~ "allspice",
      ingredient_std %in% c("fresh squeezed orange juice", "orange juice") ~ "orange_juice",
      ingredient_std %in% c("grated lemon zest", "lemon rind") ~ "lemon_zest",
      ingredient_std %in% c("lemon juice") ~ "lemon_juice",
      ingredient_std %in% c("dried tart cherry", "dried tart cherries", "dried cherries", "jars maraschino cherries", "maraschino cherry") ~ "cherries",
      ingredient_std %in% c("ground cardamom", "pinch ground cardamom") ~ "cardamom",
      ingredient_std %in% c("unsweetened chocolate", "unsweetened chocolate squares", "unsweetened baking chocolate", "baking chocolate", "dark chocolate", "bittersweet chocolate", "fine-quality bittersweet chocolate", "fine quality bittersweet chocolate", "premium quality dark chocolate", "good quality dark chocolate", "squares bittersweet chocolate", "one 3-ounce bar dark chocolate", "additional dark chocolate", "package dark chocolate", "chopped bittersweet chocolate", "ghirardelli bittersweet baking chocolate") ~ "dark_chocolate_chunks",
      ingredient_std %in% c("dried cranberries", "dried sweetened cranberries", "dried fruit juice sweetened cranberries", "craisins", "package ocean spray original craisians dried sweetened cranberries") ~ "dried_cranberries",
      ingredient_std %in% c("peanuts", "1 cup peanuts", "unsalted peanuts", "unsalted dry roasted peanuts", "dry-roasted unsalted peanuts", "coarsely chopped peanuts") ~ "peanuts",
      ingredient_std %in% c("yogurt", "yoghurt", "plain nonfat yogurt", "nonfat plain yogurt", "plain fat free greek yogurt", "fat free greek yogurt") ~ "yogurt",
      ingredient_std %in% c("cream", "heavy cream", "whipping cream", "half-and-half") ~ "cream",
      ingredient_std %in% c("fat free sour cream", "nonfat sour cream", "sour cream") ~ "sour_cream",
      ingredient_std %in% c("package cream cheese", "packages cream cheese", "cream cheese") ~ "cream_cheese",
      ingredient_std %in% c("can sweetened condensed milk", "can fat-free sweetened condensed milk", "sweetened condensed milk") ~ "condensed_milk",
      ingredient_std %in% c("box instant vanilla pudding", "package vanilla instant pudding mix", "packages vanilla instant pudding mix", "packages instant vanilla pudding", "package instant vanilla pudding", "instant vanilla pudding", "vanilla instant pudding mix", "package jello instant vanilla pudding", "package jell-o french vanilla instant pudding", "package french vanilla instant pudding", "french vanilla pudding mix") ~ "vanilla_pudding_mix",
      ingredient_std %in% c("package instant chocolate pudding mix", "packet instant chocolate pudding mix", "packages chocolate fudge pudding mix") ~ "chocolate_pudding_mix",
      ingredient_std %in% c("package butterscotch pudding mix", "package instant butterscotch pudding mix", "butterscotch pudding mix") ~ "butterscotch_pudding_mix",
      ingredient_std %in% c("box yellow cake mix", "package yellow cake mix", "yellow cake mix", "package duncan hines yellow cake mix", "bisquick baking mix") ~ "yellow_cake_mix",
      ingredient_std %in% c("box white cake mix", "package white cake mix") ~ "white_cake_mix",
      ingredient_std %in% c("box spice cake mix") ~ "spice_cake_mix",
      ingredient_std %in% c("dark fudge cake mix", "package fudge cake mix", "box betty crocker supermoist devil's food cake mix") ~ "chocolate_cake_mix",
      ingredient_std %in% c("corn flakes", "corn flakes cereal", "crushed corn flakes") ~ "corn_flakes",
      ingredient_std %in% c("rice krispies", "kellogg's rice krispies cereal") ~ "rice_krispies",
      ingredient_std %in% c("chopped macadamia nuts", "coarsely chopped macadamia nuts", "chopped dry roasted macadamia nuts", "macadamia nuts") ~ "macadamia_nuts",
      ingredient_std %in% c("corn syrup", "light corn syrup", "golden syrup") ~ "corn_syrup",
      ingredient_std %in% c("vegetable shortening", "all-vegetable shortening", "solid shortening", "solid crisco shortening", "plain crisco shortening", "stick vegetable shortening", "crisco", "golden crisco", "butter flavor shortening", "butter flavoured shortening", "crisco butter shortening", "butter flavor crisco") ~ "shortening",
      ingredient_std %in% c("pure maple syrup", "maple syrup") ~ "maple_syrup",
      ingredient_std %in% c("creamy unsalted hazelnut butter") ~ "hazelnut_butter",
      ingredient_std == "of broken up thin pretzel stick" ~ "pretzel_sticks",
      ingredient_std %in% c("regular grind coffee", "instant coffee", "instant coffee granules", "instant coffee powder", "instant espresso powder", "instant espresso", "instant espresso coffee powder", "espresso powder") ~ "espresso_powder",
      ingredient_std %in% c("slivered almonds", "whole almond", "almonds", "1 1/2 cups almonds", "sliced almonds", "ground almonds") ~ "almonds",
      ingredient_std %in% c("orange", "orange zest", "fresh", "grated orange rind", "orange rind", "orange peel", "peel of 1 orange", "finely grated fresh orange zest", "small orange") ~ "orange_zest",
      ingredient_std %in% c("flax seed", "ground flax seeds", "flax seed meal", "ground flax seed meal", "flax seeds") ~ "flaxseed",
      ingredient_std %in% c("andes mint chip", "creme de menthe baking chips", "nestlé® toll house® delightfulls mint filled morsels", "mint chocolate chips", "crushed andes mints candies") ~ "mint_chocolate_chips",
      ingredient_std %in% c("toffee pieces", "bag toffee pieces", "package toffee pieces", "brickle bits", "butter brickle", "skor toffee pieces", "skor english toffee bit") ~ "toffee",
      ingredient_std == "grade a clover honey" ~ "honey",
      ingredient_std %in% c("agave nectar", "organic agave nectar") ~ "agave_nectar",
      ingredient_std == "butterscotch chips" ~ "butterscotch_chips",
      ingredient_std %in% c("peanut butter chips", "package reese's peanut butter and milk chocolate chips", "nestle toll house peanut butter and milk chocolate chips") ~ "peanut_butter_chips",
      ingredient_std == "peppermint extract" ~ "peppermint_extract",
      ingredient_std == "cream of tartar" ~ "cream_of_tartar",
      ingredient_std == "orange extract" ~ "orange_extract",
      ingredient_std %in% c("package refrigerated chocolate chip cookie dough", "packages refrigerated chocolate chip cookie dough", "package of pillsbury refrigerated chocolate chip cookie dough", "chocolate chip cookies") ~ "refrigerated_cookie_dough",
      ingredient_std %in% c("package dry chocolate chip cookie mix", "package easy bake cookie mix") ~ "cookie_mix",
      ingredient_std == "almond extract" ~ "almond_extract",
      ingredient_std == "maple extract" ~ "maple_extract",
      ingredient_std %in% c("kahlua", "tia maria", "coffee-flavored liqueur") ~ "coffee_liqueur",
      ingredient_std %in% c("dark rum", "rum") ~ "rum",
      ingredient_std %in% c("frangelico") ~ "hazelnut_liqueur",
      ingredient_std %in% c("grand marnier") ~ "orange_liqueur",
      ingredient_std %in% c("almond flavored liqueur") ~ "almond_liqueur",
      ingredient_std %in% c("cherry flavored liqueur") ~ "cherry_liqueur",
      ingredient_std %in% c("bailey's irish cream") ~ "cream_liqueur",
      ingredient_std %in% c("1 ounce godiva chocolate liqueur") ~ "chocolate_liqueur",
      ingredient_std %in% c("chocolate syrup") ~ "chocolate_syrup",
      ingredient_std %in% c("xanthan gum") ~ "xanthan_gum",
      ingredient_std %in% c("wheat germ") ~ "wheat_germ",
      ingredient_std %in% c("chocolate candy bars", "chocolate-flavored candy coating", "milk chocolate with peanuts", "sugar-free milk chocolate candy bars", "milk chocolate candy bars", "hershey chocolate candy bars", "hershey bars", "grated hershey chocolate candy bars", "peanut butter cups", "m&m's plain chocolate candy", "bag of reeses peanut butter cups", "dozen hershey chocolate kisses", "small heath candy bars", "skor candy bars") ~ "candy_bars",
      ingredient_std %in% c("instant malted milk powder", "nonfat dry milk powder", "powdered milk", "nonfat dry milk powder") ~ "milk_powder",
      ingredient_std %in% c("matzo meal", "matzo cake meal", "matzo farfel") ~ "matzo_meal",
      ingredient_std %in% c("1/3 cup almond butter", "unsalted almond butter") ~ "almond_butter",
      ingredient_std %in% c("dry buttermilk", "buttermilk") ~ "buttermilk",
      ingredient_std %in% c("apple cider vinegar", "cider vinegar") ~ "apple_cider_vinegar",
      ingredient_std == "prune puree" ~ "prune_puree",
      ingredient_std == "butter flavor extract" ~ "butter_flavor_extract",
      ingredient_std == "cayenne pepper" ~ "cayenne_pepper",
      ingredient_std == "chia seeds" ~ "chia_seeds",
      ingredient_std == "coconut flour" ~ "coconut_flour",
      ingredient_std == "graham cracker crumbs" ~ "graham_cracker_crumbs",
      ingredient_std == "hazelnut extract" ~ "hazelnut_extract",
      ingredient_std == "lemon extract" ~ "lemon_extract",
      ingredient_std == "miniature marshmallow" ~ "marshmallow",
      ingredient_std == "pastry flour" ~ "pastry_flour",
      ingredient_std == "potato starch" ~ "potato_starch",
      ingredient_std == "raspberry jam" ~ "raspberry_jam",
      ingredient_std == "almond milk" ~ "almond_milk",
      ingredient_std %in% c("apple pie spice", "pumpkin pie spice") ~ "pie_spice_blends",
      ingredient_std %in% c("baby carrots", "carrot") ~ "carrots",
      ingredient_std == "bacon bits" ~ "bacon",
      ingredient_std == "barley flour" ~ "barley_flour",
      ingredient_std == "brewer's yeast" ~ "yeast",
      ingredient_std == "buckwheat groats" ~ "buckwheat_groats",
      ingredient_std == "butterscotch extract" ~ "butterscotch_extract",
      ingredient_std == "can chickpeas" ~ "chickpeas",
      ingredient_std == "can seamless crescent rolls" ~ "crescent_rolls",
      ingredient_std == "can white beans" ~ "white_beans",
      ingredient_std %in% c("can white decorating icing", "vanilla frosting", "container cream cheese frosting", "aerosol can white decorating icing") ~ "frosting",
      ingredient_std %in% c("candy cane", "coarsely chopped peppermint candy cane", "crushed candy cane") ~ "candy_cane",
      ingredient_std %in% c("caramel candies", "caramel ice cream topping", "kraft caramels") ~ "caramel",
      ingredient_std == "carton frozen whipped topping" ~ "cool_whip",
      ingredient_std == "chickpea flour" ~ "chickpea_flour",
      ingredient_std == "chinese chili oil" ~ "chili_oil",
      ingredient_std %in% c("chopped apricot", "chopped dried apricot", "dried apricot") ~ "apricot",
      ingredient_std == "chopped dates" ~ "dates",
      ingredient_std == "cinnamon sugar" ~ "cinnamon_sugar",
      ingredient_std == "cocoa nibs" ~ "cocoa_nibs",
      ingredient_std %in% c("coconut milk powder", "light coconut milk", "coconut milk") ~ "coconut_milk",
      ingredient_std == "coconut sugar" ~ "coconut_sugar",
      ingredient_std %in% c("crushed cereal", "granola cereal", "crushed all-bran cereal", "oat bran", "frosted mini-wheats cereal") ~ "cereal",
      ingredient_std == "custard powder" ~ "custard_powder",
      ingredient_std == "diced habaneros" ~ "habanero",
      ingredient_std == "dried chipotle powder" ~ "chipotle_powder",
      ingredient_std == "fresh garlic cloves" ~ "garlic",
      ingredient_std == "golden raisin" ~ "raisins",
      ingredient_std %in% c("low-fat ricotta cheese", "low fat cottage cheese") ~ "cheese",
      ingredient_std == "malted barley syrup" ~ "barley_malt_syrup",
      ingredient_std == "marshmallow extract" ~ "marshmallow_extract",
      ingredient_std == "orange marmalade" ~ "orange_marmalade",
      ingredient_std %in% c("organic spelt", "spelt flour") ~ "spelt_flour",
      ingredient_std %in% c("pint vanilla ice cream", "scoop ice cream") ~ "ice_cream",
      ingredient_std == "potato chips" ~ "potato_chips",
      ingredient_std %in% c("small zucchini", "finely shredded zucchini") ~ "zucchini",
      ingredient_std == "sweet potato" ~ "sweet_potato",
      ingredient_std == "toasted pine nuts" ~ "pine_nuts",
      ingredient_std == "wheat bran" ~ "wheat_bran",
      ingredient_std == "carob chips" ~ "carob_chips",
      ingredient_std == "white vinegar" ~ "white_vinegar",
      ingredient_std %in% c("almond brickle chips", "almond butter brickle") ~ "almond_brickle",
      ingredient_std %in% c("packet no-sugar-added hot chocolate mix") ~ "hot_chocolate_mix",
      TRUE ~ ingredient_std))

ingredient_data %>%
  count(ingredient_final, sort = TRUE) %>%
  print(n = 500)
