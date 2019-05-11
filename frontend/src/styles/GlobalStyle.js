import { createGlobalStyle } from "styled-components";

export const GlobalStyle = createGlobalStyle`
  :root{
    --background: ${({ theme }) => theme.white}
    --red:${({ theme }) => theme.red};
    --darkBlue:${({ theme }) => theme.darkBlue};
  }

  html {
    font-family: 'Fira Sans', sans-serif;
    box-sizing: border-box;
    font-size: 10px;
    scrollbar-width: none;
  }

  *, *:before, *:after {
    box-sizing: inherit;
  }

  body {
    height: 100%;
    margin:0;
    padding:0;
    font-size: 2rem;
    line-height: 2;
    color: ${({ theme }) => theme.black};
    background: ${({ theme }) => theme.white};
    scrollbar-width: none;
  }

  h1 {
    margin: 0;
  }

  a {
    text-decoration: none;
    color: ${({ theme }) => theme.darkBlue};
  }

  a:hover {
    color: ${({ theme }) => theme.red};
    }

  a:focus, a:active {
    outline: none;
    color:${({ theme }) => theme.darkBlue};
    border-bottom: 1px solid ${({ theme }) => theme.darkBlue}
  }

  ul li {
  color: ${({ theme }) => theme.black}; 
  list-style: none;

  &::before {
      color: ${({ theme }) => theme.red};  
      content: "•"; 
      font-size: 1em; 
      padding-right: 1.1225em; 
      position: relative;
      top: 0em; 
    }
  }

  html, body{
    &::-webkit-scrollbar {
      display: none;
   }
  }

`;

export default GlobalStyle;
