export default [
  {
    input: 'main.js',
    output: {
      file: 'main-bundle.js',
      name: 'Compiler',
      format: 'iife'
    }
  },
  {
    input: 'test.js',
    output: {
      file: 'test-bundle.js',
      name: 'Test',
      format: 'iife'
    }
  }
];
