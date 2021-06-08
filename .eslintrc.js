module.exports = {
    "env": {
        "browser": true,
        "es2020": true
    },
    "extends": [
        "eslint:recommended",
        "plugin:@typescript-eslint/recommended"
    ],
    "parser": "@typescript-eslint/parser",
    "parserOptions": {
        "ecmaVersion": 11,
        "sourceType": "module"
    },
    "plugins": [
        "@typescript-eslint"
    ],
    "rules": {
        "no-unreachable-loop":1,
        "no-eval":1,
        "no-eq-null":1,
        "no-sequences":1,
        "no-void":1,
        "vars-on-top":1,
        "no-warning-comments":0,
        "prefer-regex-literals":0,
        "no-useless-concat":1,
        "no-return-await":1,
        "no-multi-str":1,
        "no-magic-numbers":0,
        "accessor-pairs":1,
        "no-template-curly-in-string":1,
        "no-console":1,
        "eqeqeq":1,
        "curly":1
    }
};
