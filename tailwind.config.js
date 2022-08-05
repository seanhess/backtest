module.exports = {
  content: {
    // files: ['./app/**/*.hs'],
    files: ['./app/Backtest/Test.hs'],
    extract: {
      hs: (content) => {
        let start = /[\(\.,\)^]/
        let end = /\.,\)$/
        let space = /\s*/
        let utility = /([a-zA-Z0-9\s_\(\:\|]+)/

        let regex = new RegExp(start.source + space.source + utility.source + space.source, 'g')
        // let regex = /\s([a-z]+)\s/g
        // this content is called for each line!
        // gap S2 -> gap-2
        // let regex = /[\(\.\,)^]\s*([^<>",'\.`=]+)\s*[\)\.\,)$]*/g
        // let matches = [...content.matchAll(regex)] || []
        let matches = content.matchAll(regex)

        var utils = []
        // console.log("INPUT:", content)
        for (const match of matches) {
          let clean = cleanName(match[1].trim())
          if (clean) {
            utils.push(clean)
          }
        }
        console.log(utils)

        // for (const match of matches) {
        //   console.log(match)
        // }

        // let clean = results.map(function(m) {

        //   return m
        //     .trim()
        //     // .replace(/\s/g, "-")
        //     .replace(values, function(n1, n2) {
        //       console.log("WOO", n1)
        //       if (n2) {
        //         return n1 + '.' + n2
        //       }
        //       return n1
        //     })
        // })

        // console.log(clean)

        // returns an array of matches
        // return clean
        return utils
      }
    }
  },
}

function cleanName(s) {
  return s.split(/[\s\(\)]+/).map(cleanTerm).join('-')
}

function cleanTerm(t) {
  // drop prefixes (uppercase)
  let clean = removePrefix(t).toLowerCase()

  // special cases. Should we rename the function to p?, w, etc?
  if (clean == "pad") return "p"
  return clean
}

function removePrefix(s) {
  return s.replace(/^[A-Za-z]+([0-9]+)_?([0-9]+)?/, function (m, n1, n2) {
    if (n2) {
      return n1 + "." + n2
    }
    return n1
  })
}