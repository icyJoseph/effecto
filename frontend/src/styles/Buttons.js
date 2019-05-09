import styled, { css } from "styled-components";
import theme from "./theme";

const base = css`
  font-size: 3rem;
`;

export const LinkButton = styled.a`
  ${base}
  color: ${({ color }) => theme[color] || theme.black};
`;

export const ActionButton = styled.a`
  ${base}
  color: ${({ color }) => theme[color] || theme.black};
`;
