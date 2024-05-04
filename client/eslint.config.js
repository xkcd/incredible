import eslint from '@eslint/js'
import * as reactQuery from '@tanstack/eslint-plugin-query'
import hooksPlugin from 'eslint-plugin-react-hooks'
import jsxRuntime from 'eslint-plugin-react/configs/jsx-runtime.js'
import reactRecommended from 'eslint-plugin-react/configs/recommended.js'
import tseslint from 'typescript-eslint'

export default tseslint.config(
  { ignores: ['*.config.js', 'loaders/*'] },
  { files: ['src/*'] },
  eslint.configs.recommended,
  ...tseslint.configs.recommendedTypeChecked,
  reactRecommended,
  jsxRuntime,
  {
    plugins: { '@tanstack/query': reactQuery },
    rules: reactQuery.configs.recommended.rules,
  },
  {
    plugins: { 'react-hooks': hooksPlugin },
    rules: hooksPlugin.configs.recommended.rules,
  },
  {
    rules: {
      '@typescript-eslint/no-unused-vars': [
        'error',
        {
          args: 'all',
          argsIgnorePattern: '^_',
          caughtErrors: 'all',
          caughtErrorsIgnorePattern: '^_',
          destructuredArrayIgnorePattern: '^_',
          varsIgnorePattern: '^_',
        },
      ],
      'react/no-unknown-property': ['error', { ignore: ['css'] }],
      'react-hooks/exhaustive-deps': [
        'warn',
        {
          additionalHooks:
            '(useRapierEffect|useRigidBody|useCollider|useLoopHandler)',
        },
      ],
    },
  },
  {
    languageOptions: {
      parserOptions: {
        project: true,
        tsconfigRootDir: import.meta.dirname,
      },
    },
  },
)
