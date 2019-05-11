import styled, { css } from "styled-components";
import theme from "./theme";

const base = css`
  font-size: 3rem;
  cursor: pointer;
`;

export const LinkButton = styled.a`
  ${base}
  color: ${({ color }) => theme[color] || theme.black};
`;

export const ActionButton = styled.a`
  ${base}
  color: ${({ color }) => theme[color] || theme.black};
`;

export const Button = styled.button`
  ${base}
  font-size: 3.5rem;
  background: transparent;
  border: none;
  color: ${({ color, enabled }) =>
    enabled ? theme.darkBlue : theme[color] || theme.black};
  cursor: ${({ enabled }) => (enabled ? "pointer" : "unset")};
`;
