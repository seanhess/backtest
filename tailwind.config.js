module.exports = {
  content: {
    files: ['./app/Backtest/**/*.hs', './app/Backtest/*.hs'],

    // files: ['./app/**/*.hs'],

    // Custom layout functions: col, row, etc won't be automatically detected
    safelist: ['flex', 'flex-row', 'flex-col', 'grow'],

    // Custom
    extract: {
      hs: (content) => {
        let start = /[\(\.,\)^]/
        // let end = /\.,\)$/
        let space = /\s*/
        let utility = /([a-zA-Z0-9\s_\(\:\|]+)/

        let regex = new RegExp(start.source + space.source + utility.source + space.source, 'g')
        let matches = content.matchAll(regex)

        var utils = []
        // console.log("INPUT:", content)
        for (const match of matches) {
          let clean = cleanName(match[1].trim())
          if (clean) {
            utils.push(clean)
          }
        }
        // if (utils.length) console.log(utils)

        return utils
      }
    }
  },
}

// splits on whitespace (and parens) into terms, cleans terms, then combines with hyphen
function cleanName(s) {
  return s.split(/[\s\(\)]+/)
    .map(cleanTerm)
    .filter((x) => x != '')
    .join('-')
}

function cleanTerm(t) {
  let clean = camelToKebab(mapTermValue(t)).toLowerCase()
  return clean
}

function mapTermValue(t) {
  if (valueMap[t] !== undefined)
    return valueMap[t]
  else
    return t
}

const valueMap = {
  "R1_2":"1/2",
  "R1_3":"1/3",
  "R2_3":"2/3",
  "R1_4":"1/4",
  "R3_4":"3/4",
  "Px":"px",
  "S0":"0",
  "S0_5":"0.5",
  "S1":"1",
  "S1_5":"1.5",
  "S2":"2",
  "S2_5":"2.5",
  "S3":"3",
  "S3_5":"3.5",
  "S4":"4",
  "S5":"5",
  "S6":"6",
  "S7":"7",
  "S8":"8",
  "S9":"9",
  "S10":"10",
  "S11":"11",
  "S12":"12",
  "S14":"14",
  "S16":"16",
  "S20":"20",
  "S24":"24",
  "S28":"28",
  "S32":"32",
  "S36":"36",
  "S40":"40",
  "S44":"44",
  "S48":"48",
  "S52":"52",
  "S56":"56",
  "S60":"60",
  "S64":"64",
  "S72":"72",
  "S80":"80",
  "S96":"96",
  "Xs":"xs",
  "Base":"base",
  "Xl4":"4xl",
  "Xl5":"5xl",
  "Xl6":"6xl",
  "Xl7":"7xl",
  "Xl8":"8xl",
  "Xl9":"9xl",
  "Sm":"sm",
  "Md":"md",
  "Lg":"lg",
  "Xl":"xl",
  "Xl2":"2xl",
  "Xl3":"3xl",
  "B0":"0",
  "B1":"",
  "B2":"2",
  "B4":"4",
  "B6":"6",
  "B8":"8",
  "R0":"0",
  "R1":"1",
  "R2":"2",
  "R3":"3",
  "R6":"6",
  "R12":"12",
  "R45":"45",
  "R90":"90",
  "R180":"180",
  "D75":"75",
  "D100":"100",
  "D150":"150",
  "D200":"200",
  "D300":"300",
  "D500":"500",
  "D700":"700",
  "D1000":"1000",
  "Z0":"0",
  "Z10":"10",
  "Z20":"20",
  "Z30":"30",
  "Z40":"40",
  "Z50":"50",
  "O0":"0",
  "O5":"5",
  "O10":"10",
  "O20":"20",
  "O25":"25",
  "O30":"30",
  "O40":"40",
  "O50":"50",
  "O60":"60",
  "O70":"70",
  "O75":"75",
  "O80":"80",
  "O90":"90",
  "O100":"100"
}

function camelToKebab(str) {
  // replace caps except for the first one
  return str.replace(/[A-Z]/g, letter => "-" + letter.toLowerCase()).replace(/^\-/, "")
} 